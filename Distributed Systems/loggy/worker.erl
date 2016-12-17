-module(worker).
-export([
	start/5, 
	stop/1, 
	peers/2,
    merge_and_increment/3
]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    io:format("Starting: ~w - ~w - ~w - ~w - ~w~n",[Name, Logger, Seed, Sleep,Jitter]),
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
   io:format("~w has ~w and is waiting for peers~n",[Name,self()]),
   random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name,vect:zero(), Log, Peers, Sleep, Jitter);
       stop -> ok
   end.

peers(Wrk, Peers) ->
   Wrk ! {peers, Peers}.

loop(Name,VectorTime, Log, Peers, Sleep, Jitter)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            CurrentTime = merge_and_increment(Name,VectorTime,Time),%time:merge(LamportTime,Time)+1,
            %io:format("~w received message~n",[Name]),
            Log ! {log, Name, CurrentTime, {received, Msg}},
            loop(Name, CurrentTime, Log, Peers, Sleep, Jitter);

       stop -> 
            ok; %send rest of messages to logger
            
        Error ->
           Log ! {log, Name, time, {error, Error}}

    after Wait ->
            %io:format("~w is inside after~n",[Name]),
            Selected = select(Peers),
            %io:format("~w selected ~w~n",[Name,Selected]),
            CurrentTime = vect:inc(Name, VectorTime),
            Message = {hello, random:uniform(100)},
            Selected ! {msg, CurrentTime, Message},
            jitter(Jitter),
           Log ! {log, Name, CurrentTime, {sending, Message}},
           loop(Name,CurrentTime, Log, Peers, Sleep, Jitter)
   end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).

%% compare own timestamp with timestamp of message and increment by one
%% because our event is after the one that sent the message
merge_and_increment(Name, MyTime, MessageTime) ->
    vect:inc(Name, vect:merge(MyTime,MessageTime)).

    % LAMPORT IMPLEMENTATION BELOW %
%start(Name, Logger, Seed, Sleep, Jitter) ->
%	io:format("Starting: ~w - ~w - ~w - ~w - ~w~n",[Name, Logger, Seed, Sleep,Jitter]),
%    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

%stop(Worker) ->
%    Worker ! stop.

%init(Name, Log, Seed, Sleep, Jitter) ->
%	io:format("~w has ~w and is waiting for peers~n",[Name,self()]),
%   random:seed(Seed, Seed, Seed),
%    receive
%        {peers, Peers} ->
%            loop(Name,time:zero(), Log, Peers, Sleep, Jitter);
%		stop -> ok
%	end.

%peers(Wrk, Peers) ->
%   Wrk ! {peers, Peers}.

%loop(Name,LamportTime, Log, Peers, Sleep, Jitter)->
%    Wait = random:uniform(Sleep),
%    receive
%        {msg, Time, Msg} ->
%            CurrentTime = merge_and_increment(Name,LamportTime,Time),%time:merge(LamportTime,Time)+1,
        	%io:format("~w received message~n",[Name]),
%            Log ! {log, Name, CurrentTime, {received, Msg}},
%            loop(Name, CurrentTime, Log, Peers, Sleep, Jitter);

%		stop -> 
%            ok; %send rest of messages to logger
        	
%        Error ->
%           Log ! {log, Name, time, {error, Error}}
%    after Wait ->
    		%io:format("~w is inside after~n",[Name]),
%            Selected = select(Peers),
            %io:format("~w selected ~w~n",[Name,Selected]),
%            CurrentTime = time:inc(Name, LamportTime),
%            Message = {hello, random:uniform(100)},
%            Selected ! {msg, CurrentTime, Message},
%            jitter(Jitter),
%			Log ! {log, Name, CurrentTime, {sending, Message}},
%			loop(Name,CurrentTime, Log, Peers, Sleep, Jitter)
%	end.

%select(Peers) ->
%    lists:nth(random:uniform(length(Peers)), Peers).

%jitter(0) -> ok;
%jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).

%merge_and_increment(Name, MyTime, MessageTime) ->
%    time:inc(Name, time:merge(MyTime,MessageTime)).

