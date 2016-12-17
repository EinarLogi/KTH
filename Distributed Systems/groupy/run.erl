-module(run).

-export([
	t/0
]).

-define(version, gms4).

t() ->
	W1 = test:first(1, ?version, 1000), % start leader
	timer:sleep(500),% take a little nap
	test:add(2, ?version, W1, 1000),
	timer:sleep(500),% take a little nap
	test:add(3, ?version, W1, 1000),
	test:add(4, ?version, W1, 1000),
	test:add(5, ?version, W1, 1000),
	%timer:sleep(500),% take a little nap
	%test:add(4, ?version, W1, 1000),
	%test:more(3,?version, 1000), %% start N slaves(first argument nr of slaves)
	timer:sleep(10000), %% take a long nap
	W1 ! stop. %% stop the leader and watch.