%% chapters 1 and 2 of problem description.
-module(gms1).

-export([
	start/1,
	start/2
]).

%% Start function for the leader
%% Since it is the only node in the group it will become the leader.
%% The process that starts the leader (with the init function) will
%% be its master.
start(Id) ->
    Self = self(),
    %io:format("This is PID of process starting the leader: ~w~n",[self()]),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

%% Start function for a slave.
%% The slaves master is the process that starts it.
start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.


%% Init function for the leader.
%% The leader is initialized with an id, the master PID,
%% empty set of slaves, and the master PID the only one in the Group.
init(Id, Master) ->
	io:format("This is PID of leader: ~w~n",[self()]),
    leader(Id, Master, [], [Master]).

%% Init function for a slave.
%% The slave must fist send a join message
%% to a node in the group and wait for an invitation.
%% The initial state is of course as a slave
init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
	end.


leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
		stop -> ok
	end.

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} -> %% a request from its master to multicast a message, the message is forwarded to the leader.
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} -> %% a request from the master to allow a new node to join the group, the message is forwarded to the leader.
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} -> %% a multicasted message from the leader. A message Msg is sent to the master.
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} -> %% a multicasted view from the leader. A view is delivered to the master process.
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        stop -> ok 
	end.

%% Send the Message to each slave in the list.
%% lists:foreach -> calls Fun(Elem) for each element Elem in List.
bcast(Id, Message, Slaves) ->
	lists:foreach(fun(Slave) -> Slave ! Message end, Slaves).
