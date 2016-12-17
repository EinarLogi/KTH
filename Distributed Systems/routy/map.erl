-module(map).
-export([
	new/0,
	update/3,
	reachable/2,
	all_nodes/1
]).

%% Returns an empty map (a empty list)
new() ->
	[].

%% Updates the Map to reflect that Node has 
%% directional links to all nodes in 
%% the list Links. The old entry is removed.
%% First it checks wheter the Node(city) is in the map,
%% If not it adds it to the map with it's Links(directly connected cities).
%% If the Node is present in the map we delete the entry and replace
%% it with our new list of Links.
update(Node, Links, Map) ->
	case lists:keyfind(Node, 1, Map) of
		false -> [{Node, Links} | Map];
		{Node, _} -> 
			NewMap = lists:keydelete(Node,1,Map),
			[{Node, Links} | NewMap]
	end.

%% Returns the list of nodes directly reachable 
%% from Node.
reachable(Node, Map) ->
	case lists:keyfind(Node,1,Map) of
		{Node,DirectlyReachable} -> DirectlyReachable;
		false -> []
	end.

%% Returns a list of all nodes in the map, 
%% also the ones without outgoing links. 
%% So if berlin is linked to london but london 
%% does not have any outgoing links 
%% (and thus no entry in the list), 
%% london should still be in the returned list.
all_nodes(Map) ->
	lists:foldl(fun(Tuple, Acc) -> 
		{X, Links} = Tuple,
		case lists:member(X, Acc) of
			false ->
				check_links(Links,[X | Acc]);
			true ->
				check_links(Links, Acc)
		end
	end, [], Map).

%% Helper function used in all_nodes(Map)
%% Given a list, Links, checks whether and
%% item in the list is already a member of Acc.
check_links([],Acc) -> Acc;
check_links(Links, Acc) ->
	[H | T] = Links,
	case lists:member(H, Acc) of
		false ->
			check_links(T, [H | Acc]);
		true -> 
			check_links(T, Acc)
	end.