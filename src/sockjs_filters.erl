-module(sockjs_filters).

-export([cache_for/3, h_sid/3, h_no_cache/3, xhr_cors/3,
         xhr_options_post/3, xhr_options_get/3]).

-include("sockjs_internal.hrl").

-define(YEAR, 365 * 24 * 60 * 60).

%% --------------------------------------------------------------------------

-spec cache_for(req(), headers(), service()) -> {headers(), req()}.
cache_for(Req, Headers, _Service) ->
    Expires = calendar:gregorian_seconds_to_datetime(
                calendar:datetime_to_gregorian_seconds(
                  calendar:now_to_datetime(now())) + ?YEAR),
    H = [{"Cache-Control", "public, max-age=" ++ integer_to_list(?YEAR)},
         {"Expires",       httpd_util:rfc1123_date(Expires)}],
    {H ++ Headers, Req}.

-spec h_sid(req(), headers(), service()) -> {headers(), req()}.
h_sid(Req, Headers, #service{cookie_needed=false}) ->
    {Headers, Req};
h_sid(Req, Headers, #service{cookie_needed=true, cookie_name=CookieName, cookie_value=CookieValue}) ->
    %% For sticky sessions
    {[{"Set-Cookie", CookieName ++ "=" ++ CookieValue ++ "; HttpOnly"} | Headers], Req}.

-spec h_no_cache(req(), headers(), service()) -> {headers(), req()}.
h_no_cache(Req, Headers, _Service) ->
    H = [{"Cache-Control", "no-store, no-cache, must-revalidate, max-age=0"}],
    {H ++ Headers, Req}.

-spec xhr_cors(req(), headers(), service()) -> {headers(), req()}.
xhr_cors(Req, Headers, _Service) ->
    {OriginH, Req1} = sockjs_http:header('origin', Req),
     Origin = case OriginH of
                  "null"    -> "*";
                  undefined -> "*";
                  O         -> O
              end,
    {HeadersH, Req2} = sockjs_http:header(
                             'access-control-request-headers', Req1),
    AllowHeaders = case HeadersH of
                       undefined -> [];
                       V         -> [{"Access-Control-Allow-Headers", V}]
                   end,
    H = [{"Access-Control-Allow-Origin",      Origin},
         {"Access-Control-Allow-Credentials", "true"}],
    {H ++ AllowHeaders ++ Headers, Req2}.

-spec xhr_options_post(req(), headers(), service()) -> {headers(), req()}.
xhr_options_post(Req, Headers, Service) ->
    xhr_options(Req, Headers, ["OPTIONS", "POST"], Service).

-spec xhr_options_get(req(), headers(), service()) -> {headers(), req()}.
xhr_options_get(Req, Headers, Service) ->
    xhr_options(Req, Headers, ["OPTIONS", "GET"], Service).

-spec xhr_options(req(), headers(), list(string()), service()) -> {headers(), req()}.
xhr_options(Req, Headers, Methods, _Service) ->
    H = [{"Access-Control-Allow-Methods", string:join(Methods, ", ")},
         {"Access-Control-Max-Age", integer_to_list(?YEAR)}],
    {H ++ Headers, Req}.
