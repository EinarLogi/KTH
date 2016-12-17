-module(vect).

-export([
	zero/0,
	inc/2,
	merge/2,
	leq/2,
	clock/1,
	update/3,
	safe/2
]).

%  returns an initial timestamp
zero() -> [].

% returns a timestamp incremented by a named process
inc(Name, Time) ->
	case lists:keyfind(Name, 1, Time) of
		{_,Current} ->
			lists:keyreplace(Name,1,Time, {Name, Current + 1});
		false -> [{Name, 1} | Time] %% TODO: re-visit this, needs testing.
	end.

% Merges the two timestamps
% merge all timestamps
merge([], Time) -> Time; % done
merge([{Name, Ti} | Rest], Time) -> 
	case lists:keyfind(Name, 1, Time) of
		{Name, Tj} ->
			[{Name,lists:max([Ti,Tj])} | merge(Rest, lists:keydelete(Name,1,Time))];
		false -> [{Name, Ti}| merge(Rest, Time)]
	end.

%true if the timestamp Ti is less than or equal to Tj
leq([], _) ->
    true;
leq([{Name, Ti} | Rest],Time) -> 
    case lists:keyfind(Name, 1, Time) of
    	{Name, Tj} ->
        	if
            	Ti =< Tj ->
               		leq(Rest, Time);
            	true ->
                	false
        	end;
    	false -> false
	end.

% returns a clock that can keep track of the nodes
clock(_) -> zero().
%clock(Nodes) -> lists:map(fun(X) -> {X,0} end, Nodes).

% returns a clock that has been updated given that 
% we have received a log message from a node at a given time
update(From, Time, Clock) ->
    {From, Timestamp} = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
        {From, _} ->
            lists:keyreplace(From, 1, Clock, {From, Timestamp});
        false ->
             [{From, Timestamp}|Clock]
    end.

% is it safe to log an event that happened at a given time, 
% true or false 
safe(Time, Clock) -> 
	leq(Time, Clock). 