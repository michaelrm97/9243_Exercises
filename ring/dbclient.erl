-module(dbclient).
-export([insert/2, delete/1, lookup/1, stop/0]).

insert(Key, Value) ->
  server ! {self(), insert, Key, Value},
  receive
    {_From, Reply} -> Reply
  end.

delete(Key) ->
  server ! {self(), delete, Key},
  receive
    {_From, Reply} -> Reply
  end.

lookup(Key) ->
  server ! {self(), lookup, Key},
  receive
    {_From, Reply} -> Reply
  end.

stop() ->
  server ! {self(), stop},
  receive
    {_From, Reply} -> Reply
  end.
