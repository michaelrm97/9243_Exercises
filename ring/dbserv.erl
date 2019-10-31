-module(dbserv).
-export([start/0, loop/1]).

start() ->
  Pid = spawn(?MODULE, loop, [dict:new()]),
  register(server, Pid).

loop(State) ->
  receive
    {From, insert, Key, Value} ->
      From ! ok,
      NewState = dict:store(Key, Value, State),
      loop(NewState);
    {From, delete, Key} ->
      From ! ok,
      NewState = dict:erase(Key, State),
      loop(NewState);
    {From, lookup, Key} ->
      case dict:find(Key, State) of
        {ok, Value} ->
          From ! Value;
        error ->
          From ! "Not Found"
      end,
      loop(State);
    {From, stop} ->
      From ! ok
  end.


