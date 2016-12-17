-module(logger).
-export([
	start/1, 
	stop/1
]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
   Logger ! stop.

init(Nodes) ->
    loop([], vect:clock(Nodes),0).

loop(HoldBack, Clock, MaxQueueSize) ->
    %io:format("1 Clock: ~w~n",[Clock]),
    %io:format("# -- Size of holdback queue: ~w -- #~n", [length(HoldBack)]),
    receive
        {log, From, Time, Msg} ->
            % update the clock with regards to the node 'From' and time 'Time'
            UpdatedClock = vect:update(From,Time,Clock),
            % Takes the incoming message
            % and adds it to the head of the hold-back queue
            % receives a list sorted acc based on time in the queue
        	SortedHoldBackQueue = store_and_sort(From,Time,Msg,HoldBack),

            %Watch the length of the queue and store its Max value
            Max = check_queue_size(MaxQueueSize, length(HoldBack)), 

            % Sorts the current time of the workers acc.
            SortedClock = lists:keysort(2,UpdatedClock),

            %% Takes the SortedClock and the SortedHoldBackQueue ordered acc
            %% it checks the oldest/first entry in the queue and comapres its
            %% timestamp 'Time' with the lowest timestamp on the updated clock
            %% a message is considered safe to print if its timestamp is 
            %% lower than or equal to the lowest timestamp on the current clock.
            UpdatedBackUp = check_if_safe_to_print(SortedClock, SortedHoldBackQueue),
            loop(UpdatedBackUp, UpdatedClock, Max);
        stop ->
            io:format("MaxQueueSize was: ~w~n", [MaxQueueSize]),
            %io:format("%% RECEIVED STOP %%"),
            %PrintRest = lists:keysort(2,HoldBack),%% Sort and print messages still waiting in the queue
            %io:format("PrintRest : ~w~n", [PrintRest])
			ok 
    end.

			
log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

store_and_sort(From,Time,Msg,HoldBack) ->
    Updated = [{From,Time,Msg} | HoldBack],
    lists:keysort(2, Updated).

% We don't want to work on an empty list.
check_if_safe_to_print(_, []) -> [];
check_if_safe_to_print(SortedClock, SortedHoldBackQueue) ->
    [{From,Time,Msg} | T] = SortedHoldBackQueue,
    case vect:safe(Time,SortedClock) of
        true -> 
            %io:format("$$ safe to print~n"),
            log(From,Time,Msg),
            check_if_safe_to_print(SortedClock,T);
        false ->
            SortedHoldBackQueue
    end.

%% Monitor and return the Maximum length of queue during run-time    
check_queue_size(MaxQueueSize, Current) ->
    if
        Current > MaxQueueSize ->
            Current;
        true -> MaxQueueSize

    end.
