-module(test).
-export([run/2]).
%report on your initial observations
run(Sleep, Jitter) ->
    Log = logger:start([john, paul, ringo, george]),
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    timer:sleep(1000),
    logger:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).


    %% vector test:run(1,1) max queue 5, 5
    %% vector test:run(1,300) max queue 18, 10, 19, 16
    %% vector test:run(300,1) max queue 3, 2, 2

    %% Lamport test:run(1,1) max queue 28, 28, 28
    %% Lamport test:run(1,300) max queue 18, 15, 15
    %% Lamport test:run(300,1) max queue 10, 10, 10
