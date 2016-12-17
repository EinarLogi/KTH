%% This module manages pool of worker processes
%% it's task is to:
%% 1. spawn a pool of workers
%% 2. handle an incoming connection.
%% 3. assign a request to an available worker
%% 4. manage the list of available workers by removing and adding workers.

-module(worker_manager).
-export([init/1]).

init(Workers)->
	spawn_workers(Workers).

wait_for_connection(List) ->
	receive
		{new_connection, Client} ->
			{Worker, RestOfList} = get_worker(List),
			Worker ! {serve_request, Client, self()},
			wait_for_connection(RestOfList);

		%% Return the process to the process list
		%% call ourselves again with the new list
		{done_working, WorkerProcess} ->
			NewList = return_worker(WorkerProcess, List),
			wait_for_connection(NewList);

		_Other         -> {error, unknown_msg}
	end.

%% Put Worker process back in list of available processes
return_worker(WorkerProcess, List) ->
	[WorkerProcess | List].

%% Naively just return the first worker process in the list.
get_worker([H|T]) -> {H,T}.

spawn_workers(0,List)-> 
	wait_for_connection(List);

spawn_workers(N, List) ->
	WorkerPid = spawn(worker, handle_request, []),
	spawn_workers(N-1, [WorkerPid | List]).
	
spawn_workers(N) ->	
	spawn_workers(N, []).


	


