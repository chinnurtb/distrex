-export([socket_server/0, sender/4]).

socket_server() ->
    {ok, Socket} = gen_udp:open(8789, [binary, {active,false}]),
    socket_server(Socket).

socket_server(Socket) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {Who,Port,String}} ->
            io:format("CMD: ~p~n", [String]),
            case String of
                <<"LOCK", Rest/binary>> ->
                    sender(Socket, Who, Port, lock(Rest));
                <<"UNLOCK", Rest/binary>> ->
                    sender(Socket, Who, Port, unlock(Rest));
                <<"BEAT", Rest/binary>> ->
                    tick(Rest);
                <<"CHECK", Rest/binary>> ->
                    sender(Socket, Who, Port, check(Rest));
                Else ->
                    io:format("~p~n", [Else])
            end,
            socket_server(Socket);
        Else ->
            io:format("~p~n", [Else])
    end.

sender(Socket, Who, Port, What) ->
    gen_udp:send(Socket, Who, Port, atom_to_list(What)).
