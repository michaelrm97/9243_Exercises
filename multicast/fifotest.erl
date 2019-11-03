-module(fifotest).
-export([test/1]).

test(N) ->
    % Spawn N processes and call do_test on them all
    IDs = lists:seq(1, N),
    Group = lists:map(fun(Id) -> spawn(fun() -> start(Id) end) end, IDs),
    lists:foreach(fun(Pid) -> Pid ! Group end, Group).

start(Id) ->
    receive
        Group -> do_test(Id, Group)
    end.

do_test(NodeID, Group) ->
    % init the multicast library
    fifomcast:minit(NodeID, self(), Group),

    % send some multicasts
    fifomcast:msend(Group, NodeID),
    fifomcast:msend(Group, NodeID+10),
    fifomcast:msend(Group, NodeID+20),

    % receive multicasts from everyone else
    do_receive(NodeID).

% loop forever trying to receive multicast messages
do_receive(NodeID) ->
    {From, Msg} = fifomcast:mreceive(),
    if NodeID =:= 1 ->
        io:format("received ~w (~w)~n", [Msg, From]);
        true -> ok
    end,
    do_receive(NodeID).
