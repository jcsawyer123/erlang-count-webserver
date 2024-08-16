% src/ws_handler.erl
-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    erlang:send_after(5000, self(), update),
    {ok, State}.

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(update, State) ->
    {Count, RPS} = request_counter:get_stats(),
    Json = jsx:encode(#{count => Count, rps => RPS}),
    erlang:send_after(5000, self(), update),
    {reply, {text, Json}, State}.