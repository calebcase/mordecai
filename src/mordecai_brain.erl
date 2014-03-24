-module(mordecai_brain).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1,
    report/0,
    graph/0,
    energize/1, energize/2,
    stop/0
  ]).

-include("mordecai.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  {ok, {{simple_one_for_one, 1, 60},
      [{neuron,
          {mordecai_neuron, start_link, []},
          temporary, 1000, worker, [mordecai_neuron]}
      ]}}.

report() ->
  lists:map(fun({_, Pid, _, _}) -> mordecai_neuron:report(Pid) end, supervisor:which_children(?MODULE)).

graph() ->
  io:format("digraph mordecai_brain {~n"),
  lists:map(
    fun({_, Source, _, _}) ->
        Report = mordecai_neuron:report(Source),
        if
          Report#neuron.decay < 0 -> io:format("  ~p [shape=box];~n", [Source]);
          true -> io:format("  ~p [shape=circle];~n", [Source])
        end,
        dict:map(
          fun(Target, #synapse{p_in = PI, p_out = PO}) ->
              io:format("  ~p -> ~p [label=~p, penwidth=~p];~n", [Source, Target, PI + PO, PI + PO])
          end,
          Report#neuron.outbound)
    end, supervisor:which_children(?MODULE)),
  io:format("}~n").

energize(C, _) when C =< 0 -> ok;
energize(C, E) ->
  Ns = supervisor:which_children(?MODULE),
  case Ns of
    [] -> ok;
    _ ->
      case lists:nth(random:uniform(length(Ns)), Ns) of
        {_, Picked, _, _} -> mordecai_neuron:energize(Picked, E);
        _ -> ok
      end
  end,
  energize(C - 1, E).

energize(E) -> energize(1, E).

stop() ->
  exit(whereis(?MODULE), shutdown).
