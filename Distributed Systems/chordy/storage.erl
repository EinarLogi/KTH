-module(storage).
-export([
	create/0,
	add/3,
	lookup/2,
	split/3,
	merge/2
]).

create() -> [].

add(Key, Value, Store) -> [{Key,Value} | Store].

lookup(Key, Store) ->
	lists:keyfind(Key, 1, Store).

%% Return tuple {Updated, Rest} where the Updated list
%% contains elements ranging between From to To
%% and the Rest contains those who are outside that range.
split(From, To, Store) ->
	lists:foldl(fun({Key, Value}, {Acc1, Acc2}) ->
					case key:between(Key, From, To) of
						true -> {[{Key,Value} | Acc1], Acc2};
						false -> {Acc1,[{Key, Value}| Acc2]}
					end
				end, {[], []}, Store).

merge(Entries, Store) ->
	Entries ++ Store.