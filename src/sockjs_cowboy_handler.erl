-module(sockjs_cowboy_handler).

-behaviour(cowboy_handler).

%% Cowboy http callbacks
-export([init/2, terminate/3]).

%% Cowboy ws callbacks
-export([
    websocket_handle/2,
    websocket_info/2,
    websocket_init/1,
    websocket_terminate/2
]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

init(Req, Service) ->
    case sockjs_handler:is_valid_ws(Service, {cowboy, Req}) of
        {true, _Reason} ->
            Service1 = Service#service{disconnect_delay = 5 * 60 * 1000},
            Info = sockjs_handler:extract_info({cowboy, Req}),
            SessionPid = sockjs_session:maybe_create(undefined, Service1, Info),
            RawWebSocket =
                case sockjs_handler:get_action(Service, {cowboy, Req}) of
                    {match, WS} when WS =:= websocket orelse WS =:= rawwebsocket -> WS
                end,

            {cowboy_websocket, Req, {RawWebSocket, SessionPid}};
        {false, _Reason} ->
            {cowboy, Req1} = sockjs_handler:handle_req(Service, {cowboy, Req}),
            {ok, Req1, Service}.
    end.

terminate(_Reason, _Req, _Service) -> ok.

%% --------------------------------------------------------------------------

websocket_init(
    S
) ->
    self() ! go,
    {ok, S}.

websocket_handle(
    {text, Data},
    {RawWebsocket, SessionPid} = S
) ->
    case
        sockjs_ws_handler:received(
            RawWebsocket,
            SessionPid,
            Data
        )
    of
        ok -> {[], S};
        shutdown -> {stop, S}
    end;
websocket_handle(_Unknown, S) ->
    {stop, S}.

websocket_info(
    go,
    {RawWebsocket, SessionPid} = S
) ->
    case sockjs_ws_handler:reply(RawWebsocket, SessionPid) of
        wait ->
            {[], S};
        {ok, Data} ->
            self() ! go,
            {reply, {text, Data}, S};
        {close, <<>>} ->
            {stop, S};
        {close, Data} ->
            self() ! shutdown,
            {reply, {text, Data}, S}
    end;
websocket_info(shutdown, S) ->
    {stop, S}.

websocket_terminate(
    _Reason,
    {RawWebsocket, SessionPid}
) ->
    sockjs_ws_handler:close(RawWebsocket, SessionPid),
    ok.
