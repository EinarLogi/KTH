-module(node2test).

-export([start/0]).

start() ->
	First = test:start(node2),
	%timer:sleep(400),
	test:start(node2,6, First),
	%timer:sleep(400),
	Tk = test:keys(10),
	%timer:sleep(400),
	test:add(Tk,First),
	%timer:sleep(400),
	test:check(Tk, First),
	%timer:sleep(400),
	First ! checkit.
