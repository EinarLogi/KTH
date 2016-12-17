-module(time).

-export([
	zero/0,
	inc/2,
	merge/2,
	leq/2,
	clock/1,
	update/3,
	safe/2
]).

zero() -> 0.

inc(_, T) ->  
	T + 1.

merge(Ti, Tj) -> lists:max([Ti,Tj]).

leq(Ti,Tj) ->
	Ti =< Tj.

clock(Nodes) -> lists:map(fun(X) -> {X,0} end, Nodes).

update(Node, Time, Clock) -> 
	Updated = lists:keyreplace(Node, 1, Clock, {Node, Time}),
	lists:keysort(2,Updated).

safe(Time, Clock) -> 
	%io:format("%% safe function in time was called with Time: ~w and Clock: ~w~n", [Time,Clock]),
	[{_,LowestCurrentTime} | _] = Clock,
	leq(Time,LowestCurrentTime).