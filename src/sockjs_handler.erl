-module(sockjs_handler).

-export([init_state/4]).

-export([get_action/2, is_valid_ws/2]).

-export([dispatch_req/2, handle_req/2]).

-export([extract_info/1]).

-include("sockjs_internal.hrl").

-define(SOCKJS_URL,
    "//cdn.jsdelivr.net/sockjs/1.0.3/sockjs.min.js"
).

%% --------------------------------------------------------------------------

-spec init_state(
    binary(),
    callback(),
    any(),
    [tuple()]
) -> service().

init_state(Prefix, Callback, State, Options) ->
    #service{
        prefix = binary_to_list(Prefix),
        callback = Callback,
        state = State,
        sockjs_url =
            proplists:get_value(sockjs_url, Options, ?SOCKJS_URL),
        websocket =
            proplists:get_value(websocket, Options, true),
        cookie_needed =
            proplists:get_value(cookie_needed, Options, false),
        disconnect_delay =
            proplists:get_value(disconnect_delay, Options, 5000),
        heartbeat_delay =
            proplists:get_value(heartbeat_delay, Options, 25000),
        response_limit =
            proplists:get_value(
                response_limit,
                Options,
                128 * 1024
            ),
        logger =
            proplists:get_value(
                logger,
                Options,
                fun default_logger/3
            ),
        subproto_pref =
            proplists:get_value(subproto_pref, Options)
    }.

%% --------------------------------------------------------------------------

-spec is_valid_ws(service(), req()) -> {boolean(), tuple()}.

is_valid_ws(Service, Req) ->
    case get_action(Service, Req) of
        {match, WS} when WS =:= websocket orelse WS =:= rawwebsocket ->
            valid_ws_request(Service, Req);
        _Else ->
            {false, {}}
    end.

-spec valid_ws_request(service(), req()) -> {boolean(), tuple()}.

valid_ws_request(_Service, Req) ->
    R1 = valid_ws_upgrade(Req),
    R2 = valid_ws_connection(Req),
    {R1 and R2, {R1, R2}}.

valid_ws_upgrade(Req) ->
    case sockjs_http:header(upgrade, Req) of
        undefined ->
            false;
        V ->
            case string:to_lower(V) of
                "websocket" -> true;
                _Else -> false
            end
    end.

valid_ws_connection(Req) ->
    case sockjs_http:header(connection, Req) of
        undefined ->
            false;
        V ->
            Vs = [
                string:strip(T)
                || T <- string:tokens(string:to_lower(V), ",")
            ],
            lists:member("upgrade", Vs)
    end.

-spec get_action(service(), req()) ->
    nomatch
    | {match, atom()}.

get_action(Service, Req) ->
    Dispatch = dispatch_req(Service, Req),
    case Dispatch of
        {match, {_, Action, _, _, _}} -> {match, Action};
        _Else -> nomatch
    end.

%% --------------------------------------------------------------------------

strip_prefix(LongPath, Prefix) ->
    {A, B} = lists:split(length(Prefix), LongPath),
    case Prefix of
        A ->
            {ok, B};
        _Any ->
            {error,
                io_lib:format(
                    "Wrong prefix: ~p is not ~p",
                    [A, Prefix]
                )}
    end.

-type dispatch_result() ::
    nomatch
    | {match, {send | recv | none, atom(), server(), session(), [atom()]}}
    | {bad_method, [atom()]}.

-spec dispatch_req(
    service(),
    req()
) -> dispatch_result().

dispatch_req(#service{prefix = Prefix}, Req) ->
    Method = sockjs_http:method(Req),
    LongPath = sockjs_http:path(Req),
    {ok, PathRemainder} = strip_prefix(LongPath, Prefix),
    dispatch(Method, PathRemainder).

-spec dispatch(
    atom(),
    nonempty_string()
) -> dispatch_result().

dispatch(Method, Path) ->
    lists:foldl(
        fun
            ({Match, MethodFilters}, nomatch) ->
                case Match(Path) of
                    nomatch ->
                        nomatch;
                    [Server, Session] ->
                        case lists:keyfind(Method, 1, MethodFilters) of
                            false ->
                                Methods = [
                                    K
                                    || {K, _, _, _} <-
                                           MethodFilters
                                ],
                                {bad_method, Methods};
                            {_Method, Type, A, Filters} ->
                                {match, {Type, A, Server, Session, Filters}}
                        end
                end;
            (_, Result) ->
                Result
        end,
        nomatch,
        filters()
    ).

%% --------------------------------------------------------------------------

filters() ->
    OptsFilters = [h_sid, xhr_cors, cache_for, xhr_options_post],
    %% websocket does not actually go via handle_req/3 but we need
    %% something in dispatch/2
    [
        {t("/websocket"), [{'GET', none, websocket, []}]},
        {t("/xhr_send"),                [{'POST',    recv, xhr_send,       [h_sid, h_no_cache, xhr_cors]},
                                         {'OPTIONS', none, options,        OptsFilters}]},
        {t("/xhr"),                     [{'POST',    send, xhr_polling,    [h_sid, h_no_cache, xhr_cors]},
                                         {'OPTIONS', none, options,        OptsFilters}]},
        {t("/xhr_streaming"),           [{'POST',    send, xhr_streaming,  [h_sid, h_no_cache, xhr_cors]},
                                         {'OPTIONS', none, options,        OptsFilters}]},
        {t("/jsonp_send"),              [{'POST',    recv, jsonp_send,     [h_sid, h_no_cache]}]},
        {t("/jsonp"),                   [{'GET',     send, jsonp,          [h_sid, h_no_cache]}]},
        {t("/eventsource"),             [{'GET',     send, eventsource,    [h_sid, h_no_cache]}]},
        {t("/htmlfile"),                [{'GET',     send, htmlfile,       [h_sid, h_no_cache]}]},
        {p("/websocket"), [{'GET', none, rawwebsocket, []}]},
        {p(""), [{'GET', none, welcome_screen, []}]},
        {p("/iframe[0-9-.a-z_]*.html"), [{'GET',     none, iframe,         [cache_for]}]},
        {p("/info"), [
            {'GET', none, info_test, [h_no_cache, xhr_cors]},
            {'OPTIONS', none, options, [h_sid, xhr_cors, cache_for, xhr_options_get]}
        ]}
    ].

p(S) -> fun(Path) -> re(Path, "^" ++ S ++ "[/]?$") end.

t(S) ->
    fun(Path) ->
        re(Path, "^/([^/.]+)/([^/.]+)" ++ S ++ "[/]?$")
    end.

re(Path, S) ->
    case re:run(Path, S, [{capture, all_but_first, list}]) of
        nomatch -> nomatch;
        {match, []} -> [dummy, dummy];
        {match, [Server, Session]} -> [Server, Session]
    end.

%% --------------------------------------------------------------------------

-spec handle_req(service(), req()) -> req().

handle_req(Service = #service{logger = Logger}, Req) ->
    Logger(Service, Req, http),
    Dispatch = dispatch_req(Service, Req),
    handle(Dispatch, Service, Req).

handle(nomatch, _Service, Req) ->
    sockjs_http:reply(404, [], "", Req);
handle({bad_method, Methods}, _Service, Req) ->
    MethodsStr = string:join(
        [
            atom_to_list(M)
            || M <- Methods
        ],
        ", "
    ),
    H = [{"Allow", MethodsStr}],
    sockjs_http:reply(405, H, "", Req);
handle(
    {match, {Type, Action, _Server, Session, Filters}},
    Service,
    Req
) ->
    {Headers, Req2} = lists:foldl(
        fun(
            Filter,
            {Headers0, Req1}
        ) ->
            sockjs_filters:Filter(Req1, Headers0)
        end,
        {[], Req},
        Filters
    ),
    case Type of
        send ->
            Info = extract_info(Req2),
            _SPid = sockjs_session:maybe_create(
                Session,
                Service,
                Info
            ),
            sockjs_action:Action(Req2, Headers, Service, Session);
        recv ->
            try
                sockjs_action:Action(
                    Req2,
                    Headers,
                    Service,
                    Session
                )
            catch
                no_session ->
                    H = sockjs_filters:h_sid(Req2, []),
                    sockjs_http:reply(404, H, "", Req2)
            end;
        none ->
            sockjs_action:Action(Req2, Headers, Service)
    end.

%% --------------------------------------------------------------------------

-spec default_logger(
    service(),
    req(),
    websocket | http
) -> no_return().

default_logger(_Service, _Req, _Type) ->
    % As the service need a function to call as default, we simply give a dummy function
    %    LongPath = sockjs_http:path(Req),
    %    Method   = sockjs_http:method(Req).
    %    io:format("~s ~s~n", [Method, LongPath]),
    ok.

-spec extract_info(req()) -> info().

extract_info(Req) ->
    Peer = sockjs_http:peername(Req),
    Sock = sockjs_http:sockname(Req),
    Path = sockjs_http:path(Req),
    Headers = lists:foldl(
        fun(H, Acc) ->
            case sockjs_http:header(H, Req) of
                undefined -> Acc;
                V -> [{H, V} | Acc]
            end
        end,
        [],
        [
            referer,
            'x-client-ip',
            'x-forwarded-for',
            'x-cluster-client-ip',
            via,
            'x-real-ip'
        ]
    ),
    [
        {peername, Peer},
        {sockname, Sock},
        {path, Path},
        {headers, Headers}
    ].
