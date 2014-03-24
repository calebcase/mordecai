mordecai_brain:energize(100, 1).
mordecai_brain:energize(100, 1).
mordecai_neuron:send(P, #transmitter{type = c, count = 5}).
mordecai_brain:energize(100, -1).


f().
mordecai:start(normal, []).
supervisor:start_link(mordecai_brain, []).

N = #neuron{spread = 4, spike = #spike{lockout = 10}}.
{ok, I} = supervisor:start_child(mordecai_brain, [N]).
{ok, O} = supervisor:start_child(mordecai_brain, [N]).
supervisor:start_child(mordecai_brain, [N]).
supervisor:start_child(mordecai_brain, [N]).
supervisor:start_child(mordecai_brain, [N]).
supervisor:start_child(mordecai_brain, [N]).
supervisor:start_child(mordecai_brain, [N#neuron{decay = 0.5}]).

mordecai_brain:energize(10, 1).

mordecai_neuron:energize(I, 1).
mordecai_neuron:send(I, #transmitter{type = c, count = 5}).
mordecai_neuron:energize(O, 1).
mordecai_neuron:send(O, #transmitter{type = c, count = 5}).

I.
O.
mordecai_brain:graph().

mordecai_brain:stop().
