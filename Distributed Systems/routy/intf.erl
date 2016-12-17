-module(intf).
-compile(export_all).

%% Return an empty set of interfaces.
new() -> [].

%% add a new entry to the set and return
%% the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
	case lists:keyfind(Name,1,Intf) of
		false -> [{Name,Ref,Pid} | Intf];
		true -> Intf
	end.
	

%% remove an entry given a name of an interface, 
%% return a new set of interfaces.
remove(Name, Intf) ->
	lists:keydelete(Name, 1, Intf).

%% find the process identifier given a name, return {ok, Pid} if 
%% found otherwise notfound.
lookup(Name, Intf) ->
	case lists:keyfind(Name, 1, Intf) of
		false -> notfound;
		{_,_,Pid} -> {ok,Pid}
			
	end.

%% find the reference given a name and return {ok, Ref} or notfound.
ref(Name, Intf) ->  
	case lists:keyfind(Name, 1, Intf) of
		false -> notfound;
		{_,Ref,_} -> {ok,Ref}			
	end.	

%% find the name of an entry given a reference and return 
%% {ok, Name} or notfound.
name(Ref, Intf) ->  
	case lists:keyfind(Ref, 2, Intf) of
		false -> notfound;
		{Name,_,_}  -> {ok,Name}		
	end.	

%% Return a list with all names.
list(Intf) ->
	[Name || {Name,_,_} <- Intf].

%% Send the message to all interface processes.
broadcast(Message, Intf) -> 
    lists:map(fun({_,_,Pid}) -> 
    			Pid ! Message 
    		end, Intf).

