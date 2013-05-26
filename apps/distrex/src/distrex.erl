-module(distrex).
-behaviour(gen_server).

%% gen_server behaviour
-export([start_link/0, start/0,code_change/3,handle_call/3,init/1,handle_cast/2,handle_info/2,terminate/2]).

%% extra
-export([heartbeat/1, heartbeat/2]).

%% API
-export([unlock/1,check/1,tick/1,lock/1,state/0,maxmiss/0, misstime/0]).

%% Test functions
-export([test/0, mimic_tick/1]).

start() ->
    register(socket_loop, spawn(socket, socket_server, [])),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
	start().

init(Args) ->
    print(Args),
    {ok, {dict:new(), dict:new()}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% We don't have any specific needs for these yet but we need to over-
%% ride them for the gen_server behaviour.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Message, Library) ->
    {noreply, Library}.
terminate(_Reason, _Library) -> ok.

%% Tells the client if the What is locked
handle_call({check, What}, From, State) ->
    {CheckoutMap, _ } = State,
    io:format("Checking ~p from ~p~n", [What, From]),
    Reply = case dict:find(What, CheckoutMap) of
                {ok, _} ->
                    inuse;
                error ->
                    free
            end,
    {reply, Reply, State};
%% Locks a resource and assigns it to the client
handle_call({add, What}, From, State) ->
    {CheckoutMap, HeartBeatMap } = State,
    io:format("Adding: ~p~n", [What]),
    {Reply,
     NewCheckoutMap,
     NewHeartBeatMap} = case dict:find(What, CheckoutMap) of
                            {ok, _} ->
                                io:format("Already exists: ~p~n", [What]),
                                {denied, CheckoutMap, HeartBeatMap};
                            error ->
                                io:format("Added: ~p~n", [What]),
                                {ok, dict:store(What, From, CheckoutMap),
                                 dict:store(What, heartbeat(What), HeartBeatMap)}
                        end,
    {reply, Reply, {NewCheckoutMap, NewHeartBeatMap}};
%% Unlocks a resource
handle_call({remove, What}, _From, State) ->
    {CheckoutMap, HeartBeatMap} = State,
    io:format("Removing: ~p~n", [What]),
    {Reply,
     NewCheckoutMap,
     NewHeartBeatMap} = case dict:find(What, CheckoutMap) of
                            {ok, _} ->
                                {ok, Monitor} = dict:find(What, HeartBeatMap),
                                Monitor ! stop,
                                {ok, dict:erase(What, CheckoutMap),
                                 dict:erase(What, HeartBeatMap)};
                            error ->
                                {novalue, CheckoutMap, HeartBeatMap}
                        end,
    {reply, Reply, {NewCheckoutMap, NewHeartBeatMap}};
%% Sends a tick to the heartbeat monitor for the locked resource
handle_call({heartbeat, What}, _From, State) ->
    {_CheckoutMap, HeartBeatMap} = State,
    Reply = case dict:find(What, HeartBeatMap) of
                {ok, Monitor} ->
                    Monitor ! beat,
                    ok;
                error ->
                    io:format("Tick for untracked resource~n", []),
                    error
            end,
    {reply, Reply, {_CheckoutMap, HeartBeatMap}};
handle_call({state}, _From, State) ->
    {reply, State, State}.

%%%%%%%%%%%%%%%%%%
%% Server Comms %%
%%%%%%%%%%%%%%%%%%
unlock(What) ->
    gen_server:call(?MODULE, {remove, What}).
check(What) ->
    gen_server:call(?MODULE, {check, What}).
tick(What) ->
    gen_server:call(?MODULE, {heartbeat, What}).
lock(What) ->
    gen_server:call(?MODULE, {add, What}).
state() ->
    gen_server:call(?MODULE, {state}).


heartbeat(What) ->
    spawn(distrex, heartbeat, [maxmiss(), What]).
heartbeat(0, What) ->
    receive
        beat ->
            heartbeat(maxmiss(), What);
        stop ->
            ok
    after
		misstime()->
            unlock(What)
    end;
heartbeat(N, What) ->
    receive
        beat ->
            heartbeat(maxmiss(), What);
        stop ->
            ok
    after
		misstime()->
            heartbeat(N-1, What)
    end.

mimic_tick(What) ->
    tick(What),
    receive
    after 3000 ->
            mimic_tick(What, 10)
    end.
mimic_tick(_, 0) ->
    io:format("Finishing~n", []),
    finishing_tick;
mimic_tick(What, N) ->
    io:format("Tick: ~p~n", [What]),
    tick(What),
    receive
    after 3000 ->
            mimic_tick(What, N-1)
    end.

%%%%%%%%%%%%%%%%%%
%%  TEST CODE   %%
%%%%%%%%%%%%%%%%%%
test() ->
    testOK(1).
testOK(50) ->
    testFail(51);
testOK(N) ->
    lock(N),
    spawn(checkout, mimic_tick, [N]),
    testOK(N+1).
testFail(100) ->
    testDenied(101);
testFail(N) ->
    lock(N),
    testFail(N+1).
testDenied(150) ->
    ok;
testDenied(N) ->
    io:format("~p~n", [lock(random:uniform(50))]),
    testDenied(N+1).

%%%%%%%%%%%%%%%%%%
%%  MISC CODE   %%
%%%%%%%%%%%%%%%%%%
print(Data) ->
    io:format("~p~n", [Data]).

maxmiss() ->
	{ok, MAXMISS} = application:get_env(max_heartbeat_miss),
	MAXMISS.
misstime() ->
	{ok, HEARTBEAT_MISS} = application:get_env(heartbeat_miss),
	HEARTBEAT_MISS.
