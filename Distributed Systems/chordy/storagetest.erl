-module(storagetest).

-export([
	start/0
]).

start() -> 
	Store = [{1, 1},{2, 2},{3, 3}, {4, 4}, {5, 5}, {6, 6},{7, 7}, {8, 8}],
	From = 2,
	To = 5,
	{Updated, Rest} = storage:split(From,To,Store),
	io:format("From = ~w~n", [From]),
	io:format("To = ~w~n", [To]),
	io:format("Updated = ~w~n", [Updated]),
	io:format("Rest = ~w~n", [Rest]).