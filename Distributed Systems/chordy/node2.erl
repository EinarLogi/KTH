-module(node2).

-export([
	start/1,
	start/2
]).

-define(Stabilize, 1000).
-define(Timeout, 10000). %% 10sec

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

%% Predecessor is set to nil because we must figure out
%% our predecessor through the stabilizing procedure.
init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create()).

node(Id, Predecessor, Successor, Store) ->
	receive
		checkit -> %% custom message for testing purposes
			io:format("Id: ~w Pred: ~w Succ: ~w, PID: ~w, Store:~w~n", [Id, Predecessor, Successor, self(),Store]),
			forward_check(Id, Successor),
			node(Id, Predecessor, Successor, Store);

		{checkit, Id} ->
			io:format("checkit done~n"),
			node(Id, Predecessor, Successor, Store);

		{checkit, Ref} -> 
			io:format("Id: ~w Pred: ~w Succ: ~w, PID: ~w, Store:~w~n", [Id, Predecessor, Successor,self(), Store]),
			forward_check(Ref,Successor),
			node(Id, Predecessor, Successor, Store);

		{key, Qref,Peer} -> %% a peer needs to know our key
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Store);

		{notify, New} -> %% a new node informs us of its existence.
			{Pred, Updated} = notify(New, Id, Predecessor, Store),
			node(Id, Pred, Successor, Updated);

		{request, Peer} -> %% a predecessor needs to know our predecessor.
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor, Store);

		{status, Pred} -> %% our successor informs us about its predecessor.
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ, Store);

		stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);

        probe -> %% Check if the ring is actually connected
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);

        {probe, Id, Nodes, T} -> %% message is back to me again.
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);

        {probe, Ref, Nodes, T} -> %% forward the message.
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);

        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
                        Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);

        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);

        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged)

	end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

%% We are responsible for keyes from predecessor +1 to (and equal) to our node.
%% case key:between(Key, Pkey, Id)
%% Param: Key is the key of the tuple to be added
%% Param: Pkey is the key of my predecessor
%% Param: Id is my Id
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id)  of
		true ->
		    Client ! {Qref, ok},
		    storage:add(Key, Value, Store); %% Return the updated store
		false ->
			Spid ! {add, Key, Value, Qref, Client}, %% forward the message to my successor
			Store %% Return store unchanged
	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client}
    end.

%% Start a message that will circle around to see if the circle is connected.
create_probe(Id, {_,Spid}) ->
	Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
	Now = erlang:system_time(micro_seconds),
	Diff = Now - T,
	AmountOfNodes = lists:flatlength(Nodes),
	io:format("------------------------------------~n"),
	io:format("The ring currently consists of ~w nodes~n", [AmountOfNodes]),
	io:format("It took the probe message ~w microseconds to go around the circle~n", [Diff]),
	io:format("------------------------------------~n").

forward_probe(Ref, T, Nodes, Id, {_,Spid}) ->
	Spid ! {probe, Ref, [Id|Nodes], T}.

forward_check(Id, {_,Spid}) ->
	Spid ! {checkit, Id}.

%% connect(Id, Peer) finds the successor for a node which is new in the ring.
%% If Peer = nil, then we are the first node.
connect(Id, nil) ->
    {ok, {Id, self()}}; %% we're the first node in the ring so we are our own successor.
connect(Id, Peer) ->
    Qref = make_ref(), %% BIF, returns a unique reference. The reference is unique among connected nodes.
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
               {ok, {Skey, Peer}}
    after ?Timeout ->
            io:format("Time out: no response~n",[])
	end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

%% Only iform our peer that sent the request about our predecessor.
request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
	end.

%% A Peer thinks that it is our predecessor.
notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
		nil -> %% We currently have no predecessor.
			Keep = handover(Id, Store, Nkey, Npid),
			{{Nkey, Npid}, Keep}; %% set the peer as our predecessor and return with the updated store
		{Pkey,  _} ->
		    case key:between(Nkey, Pkey, Id) of
				true -> 
					Keep = handover(Id, Store, Nkey, Npid), %% we're about to assign the "new" peer as our predecessor, let him also have a part of the store
					{{Nkey, Npid}, Keep}; %% the "new" peer is closer than our previous predecessor.
				false -> {Predecessor,Store} %% no changes to be made
			end
	end.

%% Give predecessor part of our store
handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

%% Param "Pred": our successors current predecessor
stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil -> %% inform our successor of our existence.
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} -> %% We are the predecessor of our successor.
			Successor;
		{Skey, _} -> %% Our successor currently points at him self as his own predecessor
			Spid ! {notify, {Id, self()}},
			Successor;
		{Xkey, Xpid} -> %% Our successor is pointing to another node as its predecessor. 
			case key:between(Xkey, Id, Skey) of
				true -> %% The other node is closer it should be our successor.
					Xpid ! {request,self()}, %% the other node should be our successor.
					Pred;
				false -> %% We are between the other node and our successor so we should be the predecessor of our successor.
					Spid ! {notify, {Id,self()}},
					Successor
			end
	end.