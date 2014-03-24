-record(synapse, {
    ref :: reference(),
    p_in = 0:: float(),
    p_out = 0:: float()
  }).

-record(transmitter, {
    type :: a | b | c,
    count = 1 :: pos_integer(),
    from = false :: false | pid()
  }).

-record(spike, {
    send = #transmitter{type = c} :: transmitter(),
    back = #transmitter{type = a} :: transmitter(),
    lockout = 1000 :: 1..4294967295,
    resting = 0.1 :: float()
  }).

-record(neuron, {
    lock = false :: true | false,
    last = now() :: stamp(),
    decay = -0.1 :: float(),
    action = 1.0 :: float(),
    potential = 0.1 :: float(),
    refresh = 100 :: 1..4294967295,
    spike = #spike{} :: spike(),
    notify = [] :: [pid()],
    inbound = dict:new() :: dict(),
    outbound = dict:new() :: dict(),
    spread = 10 :: pos_integer()
  }).

-type transmitter() :: #transmitter{}.
-type spike() :: #spike{}.
-type neuron() :: #neuron{}.

-type stamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type connection() :: {inbound | outbound, attach | detach, pid()}.
-type energy() :: {energy, float()}.
