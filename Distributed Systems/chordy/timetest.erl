-module(timetest).

-export([
	start/0
]).

start() ->
	List = [1,2,3,4,5],
	Len = lists:flatlength(List),
	io:format("List is ~w long~n", [lists:flatlength(List)]),
	%FirstTime = erlang:timestamp(),
	FirstTime = erlang:system_time(micro_seconds),
	timer:sleep(500),
	%SecondTime = erlang:timestamp(),
	SecondTime = erlang:system_time(micro_seconds),
	SecondTime - FirstTime.