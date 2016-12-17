-module(hist).
-export([
	new/1,
	update/3
]).

%% Return a new history,
%%  where messages from Name will always be seen as old.
new(Name) ->
	[{Name, 0}].


%% Check if message number N from the Node is old or new. 
%% If it is old then return old but if it new return {new, Updated}
%% where Updated is the updated history.
update(Node, N, History)->
    case lists:keyfind(Node, 1, History) of
    	{Node, TimeStamp} ->
	        if
		        N > TimeStamp -> 
					Updated = lists:keydelete(Node, 1, History),
					{new, [{Node,H}|Updated]};
		        N =< TimeStamp -> 
		            old
	        end;
	    false ->
				Updated = lists:keydelete(Node, 1, History),
				{new, [{Node,H}|Updated]}
    end.