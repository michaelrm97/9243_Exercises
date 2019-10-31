-module(ringserv).
-export([start_ring/1, stop_ring/1, loop/1]).

-record(state, {id, nodes, next=none, items=dict:new()}).

start_ring(N) ->
  Pids = start_ring(N, N),
  join_ring(hd(Pids), Pids),
  wait_for_replies(N),
  Pids.

stop_ring([]) -> ok;
stop_ring([Server|Rest]) ->
  Server ! {self(), stop},
  receive
    ok -> ok
  end,
  stop_ring(Rest).

wait_for_replies(0) -> ok;
wait_for_replies(N) ->
  receive
    ok -> ok
  end,
  wait_for_replies(N - 1).

join_ring(First, [Curr|[Next|Rest]]) ->
  Curr ! {self(), next, Next},
  join_ring(First, [Next|Rest]);
join_ring(First, [Curr]) ->
  Curr ! {self(), next, First}.

start_ring(0, _) -> [];
start_ring(Id, N) ->
  [start(Id, N) | start_ring(Id - 1, N)].

start(Id, N) ->
  spawn(?MODULE, loop, [#state{id=Id, nodes=N}]).

loop(State) ->
  receive
    {From, next, Pid} ->
      % io:format("assigning next ~p to ~B ~n", [Pid, State#state.id]),
      From ! ok,
      loop(State#state{next=Pid});
    {From, insert, Key, Value} ->
      % Check if this node is meant to handle this
      Node = erlang:phash(Key, State#state.nodes),
      NewState = 
      if Node =:= State#state.id -> 
        From ! ok,
        State#state{items=dict:store(Key, Value, State#state.items)};
        true -> 
          State#state.next ! {From, insert, Key, Value},
          State
      end,
      loop(NewState);
    {From, delete, Key} ->
      % Check if this node is meant to handle this
      Node = erlang:phash(Key, State#state.nodes),
      NewState = 
      if Node =:= State#state.id -> 
        From ! ok,
        State#state{items=dict:erase(Key, State#state.items)};
        true -> 
          State#state.next ! {From, delete, Key},
          State
      end,
      loop(NewState);
    {From, lookup, Key} ->
      % Check if this node is meant to handle this
      Node = erlang:phash(Key, State#state.nodes),
      if Node =:= State#state.id -> 
          case dict:find(Key, State#state.items) of
            {ok, Value} ->
              From ! Value;
            error ->
              From ! "Not Found"
          end;
        true -> State#state.next ! {From, lookup, Key}
      end,
      loop(State);
    {From, stop} ->
      From ! ok
  end.
