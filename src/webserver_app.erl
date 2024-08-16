%%%-------------------------------------------------------------------
%% @doc webserver public API
%% @end
%%%-------------------------------------------------------------------

-module(webserver_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(cowboy),
    
    % Start the counter process
    {ok, _} = request_counter:start_link(),
    
    Port = get_port(),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []},
            {"/ws", ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("Server started on port ~p~n", [Port]),
    webserver_sup:start_link().

stop(_State) ->
    ok.

get_port() ->
    case init:get_argument(port) of
        {ok, [[PortStr|_]]} ->
            list_to_integer(PortStr);
        _ ->
            case application:get_env(webserver, port) of
                {ok, Port} when is_integer(Port) ->
                    Port;
                _ ->
                    8080 % Default port
            end
    end.