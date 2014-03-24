%% @author Caleb Case <calebcase@gmail.com>
%% @copyright 2014 Caleb Case

-module(mordecai).
-behaviour(application).

-export([
    start/2,
    stop/1
  ]).

start(normal, _Args) ->
  mordecai_brain:start_link().

stop(_State) ->
  ok.
