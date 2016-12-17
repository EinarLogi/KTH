%% This module starts up the improved rudy server
%% it creates a listening socket on port=Port
%% and forwards new connections to the worker manager
-module(start).
-export([start_server/1,stop_server/0]).

start_server(Port) ->
    register(start_server, spawn(fun() -> init(Port) end)).

stop_server() ->
    exit(whereis(start_server), "time to die").

init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true},{keepalive, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} -> 
			%% Socket has been created, now i want to start a listening pool
			%% on the Socket.
			%% Spawn the worker manager and specify how many workers we want in the process pool.
			WorkerManager = spawn(worker_manager, init, [100]),
			handler(Listen, WorkerManager),
			gen_tcp:close(Listen),
			ok;
		{error, Error} ->
			io:format("rudy: init error: ~w~n", [Error])
	end.

handler(Listen, WorkerManager) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			WorkerManager ! {new_connection, Client},
			handler(Listen, WorkerManager);
		{error, Error} ->
			io:format("rudy: error from handler: ~w~n", [Error])
	end.