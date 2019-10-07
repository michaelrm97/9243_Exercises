-module(dbserv).
-export([start/0, insert/2, delete/1, lookup/1, stop/0, loop/1]).
% -compile(export_all).

start() ->
  Pid = spawn(?MODULE, loop, [dict:new()]),
  register(server, Pid).

insert(Name, Address) ->
  server ! {self(), insert, Name, Address},
  receive
    {_From, Reply} -> Reply
  end.

delete(Name) ->
  server ! {self(), delete, Name},
  receive
    {_From, Reply} -> Reply
  end.

lookup(Name) ->
  server ! {self(), lookup, Name},
  receive
    {_From, Reply} -> Reply
  end.

stop() ->
  server ! {self(), stop},
  receive
    {_From, Reply} -> Reply
  end.

loop(State) ->
  receive
    {From, insert, Name, Address} ->
      From ! {self(), ok},
      NewState = dict:store(Name, Address, State),
      loop(NewState);
    {From, delete, Name} ->
      From ! {self(), ok},
      NewState = dict:erase(Name, State),
      loop(NewState);
    {From, lookup, Name} ->
      case dict:find(Name, State) of
        {ok, Value} ->
          From ! {self(), Value};
        error ->
          From ! {self(), "Not Found"}
      end,
      loop(State);
    {From, stop} ->
      From ! {self(), ok}
  end.
