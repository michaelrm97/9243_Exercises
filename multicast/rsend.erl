-module(rsend).
-export([rsend/2, rsend/3]).

rsend(Pid, Msg) ->
  spawn(fun() -> do_rsend(self(), Pid, Msg) end).

rsend(Self, Pid, Msg) ->
  spawn(fun() -> do_rsend(Self, Pid, Msg) end).

do_rsend(Self, Pid, Msg) ->
  T = rand:uniform(1000),
  timer:sleep(T),
  Pid ! {Self, Msg}.
