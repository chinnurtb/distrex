-module(checkout).
-compile(export_all).
-define(MAX_HEARBEAT_MISSES, 10).
-define(HEARTBEAT_MISS, 1000).

init() ->
	register(broker, spawn(checkout, server, [])).

server() ->
	server(dict:new(), dict:new()).
server(CheckoutMap, HeartBeatMap) ->
	receive
		{check, Pid, What} ->
			io:format("Checking ~p from ~p~n", [What, Pid]),
			case dict:find(What, CheckoutMap) of
				{ok, _} ->
					Pid ! inuse;
				error ->
					Pid ! free
			end,
			server(CheckoutMap, HeartBeatMap);
		{add, Pid, What} ->
			io:format("Adding~n", []),
			case dict:find(What, CheckoutMap) of
				{ok, _} ->
					Pid ! denied;
				error ->
					Pid ! ok,
					server(dict:store(What, Pid, CheckoutMap),
						  dict:store(What, heartbeat(What), HeartBeatMap))
			end;
		{remove, Pid, What} ->
			io:format("Removing~n", []),
			case dict:find(What, CheckoutMap) of
				{ok, _} ->
					server(dict:erase(What, CheckoutMap),
						   dict:erase(What, HeartBeatMap)),
					Pid ! ok;
				error ->
					Pid ! novalue
			end;
		{heartbeat, Pid} ->
			case dict:find(Pid, HeartBeatMap) of
				{ok, Monitor} ->
					Monitor ! beat,
					server(CheckoutMap, HeartBeatMap);
				error ->
					server(CheckoutMap, HeartBeatMap)
			end;
		restart ->
			io:format("Restarting, reloading code...~n", []),
			checkout:server(CheckoutMap);
		quit ->
			ok
	end.

restart() ->
	broker ! restart.

quit() ->
	broker ! quit.

reserve(What) ->
	broker ! {check, self(), What},
	receive
		inuse ->
			inuse;
		free ->
			broker ! {add, self(), What},
			receive
				ok ->
					ok;
				denied ->
					denied
			end
	end.

tick(What) ->
	broker ! {heartbeat, What}.

heartbeat(What) ->
	spawn(checkout, heartbeat, [?MAX_HEARBEAT_MISSES, What]).
heartbeat(0, What) ->
	receive
		beat ->
			heartbeat(?MAX_HEARBEAT_MISSES, What)
	after
		?HEARTBEAT_MISS ->
			broker ! {remove, self(), What}
	end;
heartbeat(N, What) ->
	receive
		beat ->
			heartbeat(?MAX_HEARBEAT_MISSES, What)
	after
		?HEARTBEAT_MISS ->
			heartbeat(N-1, What)
	end.

mimic_tick(What) ->
	tick(What),
	receive
	after 3000 ->
			mimic_tick(What, 10)
	end.

mimic_tick(What, 0) ->
	finishing_tick;
mimic_tick(What, N) ->
	tick(What),
	receive
	after 3000 ->
			mimic_tick(What, N-1)
	end.

test() ->
	init(),
	testOK(0).

testOK(50) ->
	testFail(51);
testOK(N) ->
	reserve(N),
	spawn(checkout, mimic_tick, [N]),
	testOK(N+1).

testFail(100) ->
	testDenied(101);
testFail(N) ->
	reserve(N),
	testFail(N+1).

testDenied(150) ->
	ok;
testDenied(N) ->
	io:format("~p~n", [reserve(random:uniform(50))]),
	testDenied(N+1).
