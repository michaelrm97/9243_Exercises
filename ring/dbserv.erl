-module(dbserv).
-export([start/0, loop/1]).

start() ->
  Pid = spawn(?MODULE, loop, [dict:new()]),
  register(server, Pid).

loop(State) ->
  receive
    {From, insert, Key, Value} ->
      From ! {self(), ok},
      NewState = dict:store(Key, Value, State),
      loop(NewState);
    {From, delete, Key} ->
      From ! {self(), ok},
      NewState = dict:erase(Key, State),
      loop(NewState);
    {From, lookup, Key} ->
      case dict:find(Key, State) of
        {ok, Value} ->
          From ! {self(), Value};
        error ->
          From ! {self(), "Not Found"}
      end,
      loop(State);
    {From, stop} ->
      From ! {self(), ok}
  end.


