-module(tomtest).
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
    tomcast:minit(NodeID, self(), Group),

    % send some multicasts
    tomcast:msend(Group, NodeID),
    tomcast:msend(Group, NodeID+10),
    tomcast:msend(Group, NodeID+20),

    % receive multicasts from everyone else
    do_receive().

% loop forever trying to receive multicast messages
do_receive() ->
    {From, Msg} = tomcast:mreceive(),
    io:format("received ~w (~w)~n", [Msg, From]),
    do_receive().
