-module(dbclient).
-export([insert/2, delete/1, lookup/1, stop/0]).

insert(Key, Value) ->
  server ! {self(), insert, Key, Value},
  receive
    Reply -> Reply
  end.

delete(Key) ->
  server ! {self(), delete, Key},
  receive
    Reply -> Reply
  end.

lookup(Key) ->
  server ! {self(), lookup, Key},
  receive
    Reply -> Reply
  end.

stop() ->
  server ! {self(), stop},
  receive
    Reply -> Reply
  end.
