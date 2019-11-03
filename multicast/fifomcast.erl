-module(fifomcast).
-export([minit/3, msend/2, mreceive/0]).

%   called by every node to initialise the underlying multicast
%   implementation.  

%   NodeID - a unique id for the node
%   NodePid - the node's Pid
%   Group - a list of Pids of all the nodes in the multicast group
minit(NodeID, NodePid, Group) ->
  Pid = spawn_link(fun() -> mcast(NodeID, 0, array:from_list(lists:duplicate(length(Group), 0)), []) end),
  Pid ! {NodePid}.

%   multicast the given message to the group

%   Group - a list of Pids of all the nodes in the multicast group
%   Msg - the message to send
msend(Group, Msg) ->
  receive
    {From, server} ->
    From ! {self(), send, Group, Msg}
  end.

%   wait to receive a multicast message.  This will return a tuple
%   {From, Msg} with the next deliverable message that was multicast to
%   the group.  The messages returned are ordered according to the
%   guarantees provided by the underlying implementation (e.g, no
%   ordering, FIFO, total ordering).
mreceive() ->
  receive
    {From, server} ->
    From ! {self(), check},
    receive
      {F, M, msg} ->
        From ! {self()},
        {F, M};
      none -> % receive from other nodes
        receive
          {F, {M, N, S}} ->
            % io:format("~p~n", [Msg]),
            From ! {self(), recv, {F, {M, N, S}}},
            receive
              {F, M, msg} ->
                From ! {self()},
                {F, M};
              reorder ->
                From ! {self()},
                mreceive()
            end
        end
    end
  end.

% Start a new process which actual handles everything
mcast(NodeID, Seq, V, Q) ->
  receive
    {From} ->
      From ! {self(), server},
      mcast(NodeID, Seq, V, Q);
    {From, send, Group, Msg} ->
      lists:foreach(fun(Pid) -> rsend:rsend(From, Pid, {Msg, NodeID, Seq + 1}) end, Group),
      From ! {self(), server},
      mcast(NodeID, Seq + 1, V, Q);
    {From, recv, Msg} ->
      {F, {M, N, S}} =  Msg,
      ES = array:get(N - 1, V) + 1,
      if S =:= ES ->
        From ! {F, M, msg},
        NV = array:set(N - 1, array:get(N - 1, V) + 1, V),
        mcast(NodeID, Seq, NV, Q);
        S > ES ->
          From ! reorder,
          NQ = lists:append(Q, [{F, {M, N, S}}]),
          mcast(NodeID, Seq, V, NQ)
      end;
    {From, check} ->
      case lists:dropwhile(fun({_F, {_M, N, S}}) ->
        S =/= array:get(N - 1, V) + 1 end, Q) of
        [] ->
          From ! none,
          mcast(NodeID, Seq, V, Q);
        [{F, {M, N, S}} | _] ->
          From ! {F, M, msg},
          NQ = lists:delete({{F, M}, N, S}, Q),
          NV = array:set(N - 1, S, V),
          mcast(NodeID, Seq, NV, NQ)
      end
  end.
