-module(mordecai_neuron).
-behaviour(gen_server).

-export([
    start_link/0, start_link/1,
    send/2,
    energize/2,
    hookup/2,
    detach/2,
    report/1,
    stop/1
  ]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-include("mordecai.hrl").

%%% Client API

% Startup a default neuron.
-spec start_link() -> pid().
start_link() ->
  gen_server:start_link(?MODULE, {}, []).

% Startup a specific neuron.
-spec start_link(neuron()) -> pid().
start_link(N) ->
  gen_server:start_link(?MODULE, N, []).

% Send a transmitter to the target neuron.
-spec send(pid(), transmitter()) -> ok.
send(Pid, T) ->
  gen_server:cast(Pid, {transmitter, T}).

% Send energy to the target neuron.
-spec energize(pid(), energy()) -> ok.
energize(Pid, E) ->
  gen_server:cast(Pid, {energy, E}).

% Connect the neurons.
hookup(Source, Sink) ->
  gen_server:cast(Source, {outbound, attach, Sink}),
  gen_server:cast(Sink, {inbound, attach, Source}).

% Disconnect the neurons.
detach(Source, Sink) ->
  gen_server:cast(Source, {outbound, detach, Sink}),
  gen_server:cast(Sink, {inbound, detach, Source}).

% Return the neurons state.
report(Pid) ->
  gen_server:call(Pid, {report}).

% Shutdown a neuron.
stop(Pid) ->
  gen_server:call(Pid, stop).

%%% Server API
init({}) -> {ok, #neuron{}};
init(N = #neuron{decay = Decay}) ->
  if
    Decay > 0 ->
      erlang:send_after(N#neuron.refresh, self(), refresh),
      {ok, N};
    true -> {ok, N}
  end.

handle_call({report}, _From, N) ->
  N1 = decay(N),
  {reply, N1, N1};
handle_call(stop, _From, N) ->
  {stop, normal, ok, N}.

-spec handle_cast({transmitter, transmitter()} | connection() | energy(), neuron()) -> {atom(), neuron()}.
handle_cast({transmitter, T}, N) ->
  {noreply, process(T, N)};
handle_cast({inbound, attach, Source}, N = #neuron{inbound = I}) ->
  case dict:is_key(Source, I) of
    false ->
      Ref = monitor(process, Source),
      {noreply, N#neuron{inbound = dict:store(Source, #synapse{ref = Ref}, I)}};
    true ->
      {noreply, N}
  end;
handle_cast({inbound, detach, Source}, N = #neuron{inbound = I}) ->
  I1 = case dict:find(Source, I) of
    {ok, #synapse{ref = Ref}} ->
      demonitor(Ref, [flush]),
      dict:erase(Source, I);
    _ -> I
  end,
  {noreply, N#neuron{inbound = I1}};
handle_cast({outbound, attach, Sink}, N = #neuron{outbound = O}) ->
  case dict:is_key(Sink, O) of
    false ->
      Ref = monitor(process, Sink),
      {noreply, N#neuron{outbound = dict:store(Sink, #synapse{ref = Ref}, O)}};
    true ->
      {noreply, N}
  end;
handle_cast({outbound, detach, Sink}, N = #neuron{outbound = O}) ->
  O1 = case dict:find(Sink, O) of
    {ok, #synapse{ref = Ref}} ->
      demonitor(Ref, [flush]),
      dict:erase(Sink, O);
    _ -> O
  end,
  {noreply, N#neuron{outbound = O1}};
% Positive energy: random new outbound synapse!
handle_cast({energy, E}, N) when E > 0 ->
  SI = dict:size(N#neuron.inbound),
  if
    SI > N#neuron.spread ->
      io:format("Too many inputs: ~p~n", [SI]),
      WI = dict:fold(fun worst/3, false, N#neuron.inbound),
      case WI of
        false -> ok;
        {WIP, _} ->
          io:format("Detaching worst inbound: ~p:~p~n", [self(), WI]),
          detach(WIP, self())
      end;
    true -> ok
  end,
  SO = dict:size(N#neuron.outbound),
  if
    SO > N#neuron.spread ->
      io:format("Too many outputs: ~p~n", [SO]),
      WO = dict:fold(fun worst/3, false, N#neuron.outbound),
      case WO of
        false -> ok;
        {WOP, _} ->
          io:format("Detaching worst outbound: ~p:~p~n", [self(), WO]),
          detach(self(), WOP)
      end;
    true -> ok
  end,
  Ns = lists:filter(
    fun({_, Pid, _, _}) ->
        Pid =/= self()
    end, supervisor:which_children(mordecai_brain)),
  case Ns of
    [] -> ok;
    _ -> case lists:nth(random:uniform(length(Ns)), Ns) of
        {_, Picked, _, _} -> hookup(self(), Picked);
        _ -> ok
    end
  end,
  I = dict:map(fun zero/2, N#neuron.inbound),
  O = dict:map(fun zero/2, N#neuron.outbound),
  {noreply, N#neuron{inbound = I, outbound = O}};
% Zero energy: do nothing.
handle_cast({energy, E}, N) when E == 0 ->
  {noreply, N};
% Negative energy: remove poorest performing synapse.
handle_cast({energy, E}, N) when E < 0 ->
  WI = dict:fold(fun worst/3, false, N#neuron.inbound),
  WO = dict:fold(fun worst/3, false, N#neuron.outbound),
  case WI of
    false -> ok;
    {WIP, _} ->
      io:format("Detaching worst inbound: ~p:~p~n", [self(), WI]),
      detach(WIP, self())
  end,
  case WO of
    false -> ok;
    {WOP, _} ->
      io:format("Detaching worst outbound: ~p:~p~n", [self(), WO]),
      detach(self(), WOP)
  end,
  I = dict:map(fun zero/2, N#neuron.inbound),
  O = dict:map(fun zero/2, N#neuron.outbound),
  {noreply, N#neuron{inbound = I, outbound = O}}.

-spec handle_info(any(), neuron()) -> {atom(), neuron()}.
handle_info({'DOWN', _Ref, process, Pid, Info}, N) ->
  io:format("Connected Neuron Down: ~p~n", [Info]),
  {noreply,
    N#neuron{
      inbound = dict:erase(Pid, N#neuron.inbound),
      outbound = dict:erase(Pid, N#neuron.outbound)
    }
  };
handle_info(unlock, N) ->
  {noreply, N#neuron{lock = false}};
handle_info(refresh, N) ->
  erlang:send_after(N#neuron.refresh, self(), refresh),
  {noreply, act(decay(N))};
handle_info(Msg, N) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, N}.

-spec terminate(atom(), neuron()) -> ok.
terminate(normal, N) ->
  io:format("Neuron terminated: `~p~n", [N]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Private

% Process incoming transmitters.
-spec process(transmitter(), neuron()) -> neuron().
process(T, N) ->
  act(polarize(T, decay(N))).

% Potential for a given transmitter type.
-spec potential(transmitter()) -> float().
potential(#transmitter{type = a, count = C}) -> 0.1 * C;
potential(#transmitter{type = b, count = C}) -> 0.3 * C;
potential(#transmitter{type = c, count = C}) -> 0.7 * C.

% Transmitter potential adjustments.
-spec polarize(transmitter(), neuron()) -> neuron().
polarize(_, N = #neuron{lock = true}) -> N;
polarize(T, N) ->
  P = N#neuron.potential + potential(T),
  I = case dict:find(T#transmitter.from, N#neuron.inbound) of
    {ok, S} -> dict:store(T#transmitter.from, S#synapse{p_in = S#synapse.p_in + potential(T)}, N#neuron.inbound);
    _ -> N#neuron.inbound
  end,
  N#neuron{potential = P, inbound = I}.

% Exponential decay of potential.
-spec decay(neuron()) -> neuron().
decay(N = #neuron{lock = true}) -> N#neuron{last = now()};
decay(N) ->
  Now = now(),
  Elapsed = timer:now_diff(Now, N#neuron.last) / 1000000,
  N#neuron{last = Now, potential = N#neuron.potential * math:exp(N#neuron.decay * Elapsed)}.

% Production of the action potential spike.
-spec act(neuron()) -> neuron().
act(N = #neuron{lock = true}) -> N;
act(N = #neuron{spike = S}) when N#neuron.potential > N#neuron.action ->
  io:format("~p::Spiked: ~p:~p:~p~n",
    [
      self(),
      N#neuron.potential,
      dict:fetch_keys(N#neuron.inbound),
      dict:fetch_keys(N#neuron.outbound)
    ]),
  O = dict:map(
    fun(Sink, Synapse) ->
        send(Sink, S#spike.send#transmitter{from = self()}),
        Synapse#synapse{p_out = Synapse#synapse.p_out + potential(S#spike.send)}
    end, N#neuron.outbound),
  I = dict:map(
    fun(Source, Synapse) ->
        send(Source, S#spike.back#transmitter{from = self()}),
        Synapse#synapse{p_out = Synapse#synapse.p_out + potential(S#spike.back)}
    end, N#neuron.inbound),
  erlang:send_after(S#spike.lockout, self(), unlock),
  N#neuron{lock = true, potential = S#spike.resting, inbound = I, outbound = O};
act(N) -> N.

% Used to accumulate the worst performing synapse.
worst(Pid, S = #synapse{p_in = PI, p_out = PO}, W) ->
  case W of
    {WPid, #synapse{p_in = WPI, p_out = WPO}} ->
      WA = WPI + WPO,
      PA = PI + PO,
      if
        WA =< PA -> {WPid, W};
        WA > PA -> {Pid, S}
      end;
    _ -> {Pid, S}
  end.

% Used to reset synapse performance metrics.
zero(_, S) ->
  S#synapse{p_in = 0, p_out = 0}.
