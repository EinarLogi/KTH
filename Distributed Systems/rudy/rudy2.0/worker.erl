%% A worker gets a Client connection, reads the request
%% sends back the staic reply and returns.
-module(worker).
-export([handle_request/0]).

handle_request() ->
	receive
		{serve_request, Client, Manager} ->
			Recv = gen_tcp:recv(Client, 0),
			case Recv of
				{ok, Str} ->
					Request = http:parse_request(Str),
					Response = reply(Request),
					gen_tcp:send(Client, Response),
					Manager ! {done_working, self()},
					gen_tcp:close(Client),
					handle_request();
				{error, Error} ->
					io:format("rudy: error from request: ~w~n", [Error]);

				Other ->
					io:format("got other stuff~w~n", [Other])
			end
	end.

reply({{get, URI, _}, _, _}) ->
	timer:sleep(40),
	http:ok("<!doctype html><html><head><meta charset='utf-8'><body><h1>Welcome</h1>
		"++URI++"</body></head</html>").