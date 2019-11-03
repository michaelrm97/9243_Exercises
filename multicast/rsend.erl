-module(rsend).
-export([rsend/2, do_rsend/2]).

rsend(Pid, Msg) ->
  spawn(?MODULE, do_rsend, [Pid, Msg]).

do_rsend(Pid, Msg) ->
  T = rand:uniform(1000),
  timer:sleep(T),
  Pid ! {self(), Msg}.
