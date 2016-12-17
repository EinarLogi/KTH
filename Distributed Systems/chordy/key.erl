-module(key).

-export([
	generate/0,
	between/3
]).

generate() -> random:uniform(1000000000).

between(Key, From, To) -> 
	if
		From < To ->
			if
				Key > From, Key =< To ->
					true;
				true -> false
			end;

		true ->
			if
				Key > From, Key >= To -> true;
				Key < From, Key =< To -> true;
				true -> false
			end
	end.