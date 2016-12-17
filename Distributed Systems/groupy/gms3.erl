%% Chapter 3.2 Missing Messages

-module(gms3).

-export([
	start/1,
	start/2
]).

-define(timeout, 1000).
-define(arghh, 100).

%% Start function for the leader
%% Since it is the only node in the group it will become the leader.
%% The process that starts the leader (with the init function) will
%% be its master.
start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

%% Init function for the leader.
%% The leader is initialized with an id, the master PID,
%% empty set of slaves, and the master PID the only one in the Group.
init(Id, Rnd, Master) ->
    io:format("This is PID of leader: ~w~n",[self()]),
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master]).

%% Start function for a slave.
%% The slaves master is the process that starts it.
start(Id, Grp) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id,Rnd, Grp, Self) end)}.


%% Init function for a slave.
%% The slave must fist send a join message
%% to a node in the group and wait for an invitation.
%% The initial state is of course as a slave
init(Id, Rnd, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
        	erlang:monitor(process, Leader),
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
        after ?timeout ->
            Master ! {error, "no reply from leader"}
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
            io:format("Slave is multicsting this: ~w~n",[Msg]),
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
        {'DOWN', _Ref, process, Leader, _Reason} ->
            io:format("Slave with ID: ~w received down message~n",[Id]),
            election(Id, Master, Slaves, Group);%% Run election if leader crashes/stops.
        stop -> ok 
	end.

%% Send the Message to each slave in the list.
%% lists:foreach -> calls Fun(Elem) for each element Elem in List.
%% A leader that broadcasts will call crash(Id) with its own ID
%% for every Node in Slaves list. This is to introduce a random leader crash.
%% It could also make the processes go out of sync.
bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

%% arghh constant is 100
%% crash the current process(leader) if random number equals ?arghh
crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
    _ -> ok
end.

%% All slaves run this function if a leader crashes
%% The Master of the former leader is removed in the parameters of
%% the function with [_|Group].
%% a slave registeres its own PID to Self
%% checks the Slaves list, if its own PID is the first one in the list
%% he automatically becaumes the leader and broadcasts a new view to other slaves.
%% other slaves call for the first PID to be the leader and monitor the leader pid.
election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            io:format("This is ID: ~w and PID: ~w of new leader~n",[Id,Self]),
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
	end.

