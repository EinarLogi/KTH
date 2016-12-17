-module(routy).
-compile(export_all).

start(Reg, Name) ->
%start(Name)->
    register(Reg, spawn(fun() -> init(Name) end)).
    %register(Name, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    %io:format("got to init my PID is: ~w~n",[self()]),
    %io:format("before intf call~n",[]),
    Intf = intf:new(),
    %io:format("Intf is : ~w~n", [Intf]),
    %io:format("before map~n",[]),
    Map = map:new(),
    %io:format("before table"),
    Table = dijkstra:table(Intf, Map),
    %io:format("before Hist"),
    Hist = hist:new(Name),
    %io:format("before calling router~n",[]),
    router(Name, 0, Hist, Intf, Table, Map).

update(Router) ->
    Router ! update. %% PRÓFA!
 
broadcast(Router) ->%% PRÓFA!
    Router ! broadcast.

%sends a status message to a process, receives the reply and displays the information.
%status(RouterName) ->%% PRÓFA!
%    RouterName ! {status, self()},
%    receive
%        {status, {Name, N, Hist, Intf, Table, Map}} ->
%            io:format("-----------------------------------------~n"),
%            io:format("Got a status message from: ~w~n", [RouterName]),
%            io:format("-----------------------------------------~n"),
%            io:format("Name: ~w~n",[Name]),
%            io:format("N: ~w~n", [N]),
%            io:format("Hist: ~w~n", [Hist]),
%            io:format("Intf: ~w~n", [Intf]),
%            io:format("Table: ~w~n", [Table]),
%            io:format("Map: ~w~n", [Map]),
%            io:format("-----------------------------------------~n")
%    end.


router(Name, N, Hist, Intf, Table, Map) ->
    %io:format("waiting for message~n",[]),
    receive
        %hi -> io:format("GOT MESSAGE~n");
        {print_current_status} ->
                io:format("Current status~n"),
                io:format("Name: ~w~n", [Name]),
                io:format("~w N: ~w~n", [Name,N]),
                io:format("~w Hist: ~w~n", [Name,Hist]),
                io:format("~w Intf: ~w~n", [Name, Intf]),
                io:format("~w Table: ~w~n", [Name, Table]),
                io:format("~w Map: ~w~n", [Name, Map]),
                router(Name, N, Hist, Intf, Table, Map);

        {add, Node, Pid} ->
            %io:format("ADD WAS Called~n"),
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            %io:format("Intf1 is: ~w~n", [Intf1]),
            NewMap = map:update(Name, intf:list(Intf1),Map),
            %io:format("Int")
            router(Name, N, Hist, Intf1, Table, NewMap);

        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {'DOWN', Ref, process, _, _}  ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);

        {links, Node, R, Links} ->
           case hist:update(Node, R, Hist) of
            {new, Hist1} ->
                  intf:broadcast({links, Node, R, Links}, Intf),
                  Map1 = map:update(Node, Links, Map),
                  router(Name, N, Hist1, Intf, Table, Map1);
            old ->
                 router(Name, N, Hist, Intf, Table, Map)
            end;

        {route, Name, From, Message} ->
          io:format("~w: received message ~w from:~w~n", [Name, Message,From]),
          router(Name, N, Hist, Intf, Table, Map);

        {route, To, From, Message} ->
            io:format("~w: routing message (~w)~n", [Name, Message]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case intf:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            Pid ! {route, To, From, Message};
                        notfound ->
                            ok
                    end;
                notfound ->
                    ok end,
            router(Name, N, Hist, Intf, Table, Map);

        {send, To, Message} ->
          self() ! {route, To, Name, Message},
          router(Name, N, Hist, Intf, Table, Map);

        update ->
          Table1 = dijkstra:table(intf:list(Intf), Map),
          router(Name, N, Hist, Intf, Table1, Map);

        broadcast ->
          Message = {links, Name, N, intf:list(Intf)},
          intf:broadcast(Message, Intf),
          router(Name, N+1, Hist, Intf, Table, Map);
		
        stop ->
			ok 
	end.