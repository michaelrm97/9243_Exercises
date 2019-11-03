-module(bmtest).
-export([test/1, start/1]).

test(N) ->
    % Spawn N processes and call do_test on them all
    IDs = lists:seq(1, N),
    Group = lists:map(fun(Id) -> spawn(?MODULE, start, [Id]) end, IDs),
    lists:foreach(fun(Pid) -> Pid ! Group end, Group).

start(Id) ->
    receive
        Group -> do_test(Id, Group)
    end.

do_test(NodeID, Group) ->
    % init the multicast library
    bmcast:minit(NodeID, self(), Group),

    % send some multicasts
    bmcast:msend(Group, NodeID),
    bmcast:msend(Group, NodeID+10),
    bmcast:msend(Group, NodeID+20),

    % receive multicasts from everyone else
    do_receive().

% loop forever trying to receive multicast messages
do_receive() ->
    {From, Msg} = bmcast:mreceive(),
    io:format("received ~w (~w)~n", [Msg, From]),
    do_receive().
