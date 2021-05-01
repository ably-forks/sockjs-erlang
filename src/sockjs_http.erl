-module(sockjs_http).

-export([
    body/1,
    body_qs/1,
    callback/1,
    header/2,
    method/1,
    path/1,
    peername/1,
    sockname/1
]).

-export([chunk/2, chunk_end/1, chunk_start/3, reply/4]).

-export([
    abruptly_kill/1,
    hook_tcp_close/1,
    unhook_tcp_close/1
]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

-spec path(req()) -> string().

path({cowboy, Req}) ->
    Path = cowboy_req:path(Req),
    binary_to_list(Path).

-spec method(req()) -> atom().

method({cowboy, Req}) ->
    Method = cowboy_req:method(Req),
    method_atom(Method).

-spec method_atom(binary() | atom()) -> atom().

method_atom(<<"GET">>) -> 'GET';
method_atom(<<"T">>) -> 'GET';
method_atom(<<"PUT">>) -> 'PUT';
method_atom(<<"POST">>) -> 'POST';
method_atom(<<"ST">>) -> 'POST';
method_atom(<<"DELETE">>) -> 'DELETE';
method_atom(<<"OPTIONS">>) -> 'OPTIONS';
method_atom(<<"TIONS">>) -> 'OPTIONS';
method_atom(<<"PATCH">>) -> 'PATCH';
method_atom(<<"HEAD">>) -> 'HEAD';
method_atom('GET') -> 'GET';
method_atom('T') -> 'GET';
method_atom('PUT') -> 'PUT';
method_atom('POST') -> 'POST';
method_atom('ST') -> 'POST';
method_atom('DELETE') -> 'DELETE';
method_atom('OPTIONS') -> 'OPTIONS';
method_atom('TIONS') -> 'OPTIONS';
method_atom('PATCH') -> 'PATCH';
method_atom('HEAD') -> 'HEAD'.

-spec body(req()) -> {binary(), req()}.

body({cowboy, Req}) ->
    {ok, Body, Req1} = body({cowboy, Req}, <<"">>),
    {Body, {cowboy, Req1}}.

body({cowboy, Req}, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req1} ->
            {ok, <<Acc/binary, Data/binary>>, Req1};
        {more, Data, Req1} ->
            body({cowboy, Req1}, <<Acc/binary, Data/binary>>)
    end.

-spec body_qs(req()) -> {binary(), req()}.

body_qs(Req) ->
    {H, Req1} = header('content-type', Req),
    case H of
        H when H =:= "text/plain" orelse H =:= "" ->
            body(Req1);
        _ ->
            %% By default assume application/x-www-form-urlencoded
            body_qs2(Req1)
    end.

body_qs2({cowboy, Req}) ->
    {ok, BodyQS, Req1} =
        cowboy_req:read_urlencoded_body(Req),
    case proplists:get_value(<<"d">>, BodyQS) of
        undefined -> {<<>>, {cowboy, Req1}};
        V -> {V, {cowboy, Req1}}
    end.

-spec header(atom(), req()) ->
    {nonempty_string()
        | undefined,
        req()}.

header(K, {cowboy, Req}) ->
    H = cowboy_req:header(K, Req),
    V =
        case H of
            undefined ->
                cowboy_req:header(atom_to_binary(K, utf8), Req);
            _ ->
                H
        end,
    case V of
        undefined -> undefined;
        _ -> binary_to_list(V)
    end.

-spec callback(req()) -> {nonempty_string() | undefined, req()}.

callback({cowboy, Req}) ->
    ParsedQs = cowboy_req:parse_qs(Req),
    case lists:keyfind(<<"c">>, 1, ParsedQs) of
        false -> {undefined, {cowboy, Req}};
        {_, CB} -> {binary_to_list(CB), {cowboy, Req}}
    end.

-spec peername(req()) -> {inet:ip_address(), non_neg_integer()}.

peername({cowboy, Req}) -> cowboy_req:peer(Req).

-spec sockname(req()) -> {inet:ip_address(), non_neg_integer()}.

sockname({cowboy, Req}) -> cowboy_req:sock(Req).

%% --------------------------------------------------------------------------

-spec reply(
    non_neg_integer(),
    headers(),
    iodata(),
    req()
) -> req().

reply(Code, Headers, Body, {cowboy, Req}) ->
    Body1 = iolist_to_binary(Body),
    Req1 = cowboy_req:reply(
        Code,
        maps:from_list(enbinary(Headers)),
        Body1,
        Req
    ),
    {cowboy, Req1}.

-spec chunk_start(
    non_neg_integer(),
    headers(),
    req()
) -> req().

chunk_start(Code, Headers, {cowboy, Req}) ->
    Req1 = cowboy_req:stream_reply(
        Code,
        maps:from_list(enbinary(Headers)),
        Req
    ),
    {cowboy, Req1}.

-spec chunk(iodata(), req()) -> {ok | error, req()}.

chunk(Chunk, {cowboy, Req} = R) ->
    case cowboy_req:stream_body(Chunk, nofin, Req) of
        ok ->
            {ok, R};
        {error, _E} ->
            {error,
                %% This shouldn't happen too often, usually we
                R}
        %% should catch tco socket closure before.
    end.

-spec chunk_end(req()) -> req().

chunk_end({cowboy, Req} = R) ->
    cowboy_req:stream_body(<<"">>, fin, Req),
    R.

enbinary(L) ->
    [{list_to_binary(K), list_to_binary(V)} || {K, V} <- L].

-spec hook_tcp_close(req()) -> req().

hook_tcp_close(R = {cowboy, #{transport := T, socket := S}}) ->
    T:setopts(S, [{active, once}]),
    R.

-spec unhook_tcp_close(req()) -> req().

unhook_tcp_close(R = {cowboy, #{transport := T, socket := S}}) ->
    T:setopts(S, [{active, false}]),
    R.

-spec abruptly_kill(req()) -> req().

abruptly_kill(R = {cowboy, #{transport := T, socket := S}}) ->
    ok = T:close(S),
    R.
