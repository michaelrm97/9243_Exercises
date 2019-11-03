-module(tomcast).
-export([minit/3, msend/2, mreceive/0]).

%   called by every node to initialise the underlying multicast
%   implementation.  

%   NodeID - a unique id for the node
%   NodePid - the node's Pid
%   Group - a list of Pids of all the nodes in the multicast group
minit(NodeID, NodePid, Group) -> {NodeID, NodePid, Group}.

%   multicast the given message to the group

%   Group - a list of Pids of all the nodes in the multicast group
%   Msg - the message to send
msend(Group, Msg) -> {Group, Msg}.

%   wait to receive a multicast message.  This will return a tuple
%   {From, Msg} with the next deliverable message that was multicast to
%   the group.  The messages returned are ordered according to the
%   guarantees provided by the underlying implementation (e.g, no
%   ordering, FIFO, total ordering).
mreceive() -> ok.
