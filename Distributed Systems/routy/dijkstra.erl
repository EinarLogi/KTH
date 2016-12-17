%% computes a routing table
-module(dijkstra).
-export([
	table/2,
	route/2
]).

%% Returns the length of the shortest path 
%% to the node or 0 if the node is not found.
entry(Node, Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of
		false -> 0;
		{_, Hops, _} -> Hops
	end.

%% Replaces the entry for Node in Sorted with 
%% a new entry having a new length N and Gateway. 
%% The resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
	case entry(Node, Sorted) of
		0 -> Sorted;
		Hops -> 
			if N < Hops ->
				%io:format("I should print this out twice~n",[]),
				NewList = [{Node, N, Gateway} | lists:keydelete(Node,1,Sorted)],
				lists:keysort(2,NewList)
			; N >= Hops -> Sorted
			end
	end. 

%% Update the list Sorted given the 
%% information that Node can be reached in N hops 
%% using Gateway. If no entry is found then 
%% no new entry is added. Only if we have a better 
%% (shorter) path should we replace the existing 
%% entry.
update(Node, N, Gateway, Sorted) ->
	replace(Node,N,Gateway,Sorted). 

%% Construct a table given a sorted list of nodes, 
%% a map and a table constructed so far.
iterate(Sorted, Map, Table) -> 
	%[H | T] = Sorted,
	%{_,Length, _} = H,
	%io:format("H is =~w~n ", [H]),
	case Sorted of
		[] -> Table; % The list is done and table should be complete.
		[{_,inf,_} | _] -> Table; % Rest of the entries have inf as well.
		[{City,Hops,Gateway } | T] -> 
			%io:fwrite("lenght is not inf~n"),
			Reachable = map:reachable(City, Map), % Get the list of reachable cities
			NewSorted = lists:foldl(fun(X,Acc)-> 
										update(X, Hops+1, Gateway, Acc)
									 end, T, Reachable),
			%io:format("Reachable links from ~w are : ~w~n",[City, Reachable]),
			%NewSorted = update_links(City, N+1, Reachable,T), %% update the rest of the list
			%io:format("NewSorted = ~w~n",[NewSorted]),
			%NewTable = [{City,Gateway} | Table], %% Add the Head information from Sorted to the table.
			%io:format("NewTable is = ~w~n", [NewTable]),
			iterate(NewSorted,Map,[{City,Gateway}|Table]) %% loop with the NewSorted list which has removed the head and updated the tail.

	end.

%update_links(Gateway, N, [], Sorted)-> Sorted;
%update_links(Gateway, N, Reachable, Sorted)->
	%io:format("called test_stuff~n",[]),
%	[H|T] = Reachable,
%	NewList = update(H,N,Gateway,Sorted),
%	update_links(Gateway,N,T,NewList).

% Construct a routing table given the gate- ways and a map.
table(Gateways, Map) ->
	AllNodes = map:all_nodes(Map),
	ListFromAllNodes = lists:map(fun(City) ->
									case lists:member(City, Gateways) of
										true -> {City, 0, City};
										false -> {City, inf, unknown}
									end
								end,AllNodes),
	InitialSorted = lists:keysort(2, ListFromAllNodes),
	iterate(InitialSorted,Map,[]).




%get_gateway_list([],List) -> List;
%get_gateway_list(Gateways,List)->
%	[H|T] = Gateways,
%	get_gateway_list(T,[{H,0,H}|List]).

% Search the routing table and return the gateway 
%% suitable to route messages to a node. 
%% If a gateway is found we should return 
%% {ok, Gateway} otherwise we return notfound.
route(Node, Table) ->
    case lists:keyfind(Node,1,Table) of
		{_, Gateway} ->
	    	{ok, Gateway};
		false ->
	    	notfound
	end.


%get_new_table(Sorted) ->
%	[{X,Y} || {X,_,Y}<-Sorted].

%% dijkstra:iterate([{paris, 0, paris}, {berlin, inf, unknown}],
%%       [{paris, [berlin]}], []).
%%	returns: [{paris, paris},{berlin,paris}]

%% dijkstra:iterate([{berlin, 2, london}, {reykjavik,6,tokyo}],[{berlin,[madrid,reykjavik]}],[])
%%