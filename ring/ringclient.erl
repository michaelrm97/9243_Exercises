-module(ringclient).
-export([insert/3, delete/2, lookup/2, stop/1]).

insert(Serv, Key, Value) ->
  Serv ! {self(), insert, Key, Value},
  receive
    ok -> ok
  end.

delete(Serv, Key) ->
  Serv ! {self(), delete, Key},
  receive
    ok -> ok
  end.

lookup(Serv, Key) ->
  Serv ! {self(), lookup, Key},
  receive
    Value -> Value
  end.

stop(Serv) ->
  Serv ! {self(), stop},
  receive
    ok -> ok
  end.
