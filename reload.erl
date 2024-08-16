#!/usr/bin/env escript
%%! -sname reloader -hidden

main([Node]) ->
    Cookie = get_cookie(),
    erlang:set_cookie(node(), Cookie),
    case net_adm:ping(list_to_atom(Node)) of
        pong ->
            io:format("Connected to node ~s~n", [Node]),
            case rpc:call(list_to_atom(Node), webserver_app, reload, []) of
                ok ->
                    io:format("Hot code reload successful~n");
                {badrpc, Reason} ->
                    io:format("Hot code reload failed: ~p~n", [Reason])
            end;
        pang ->
            io:format("Failed to connect to node ~s~n", [Node])
    end.

get_cookie() ->
    case file:read_file(".erlang.cookie") of
        {ok, Cookie} ->
            string:trim(binary_to_list(Cookie));
        {error, _} ->
            io:format("Error: .erlang.cookie file not found~n"),
            halt(1)
    end.