SockJS family:

  * [SockJS-client](https://github.com/sockjs/sockjs-client) JavaScript client library
  * [SockJS-node](https://github.com/sockjs/sockjs-node) Node.js server
  * [SockJS-erlang](https://github.com/sockjs/sockjs-erlang) Erlang server


SockJS-erlang server
====================

[SockJS](http://sockjs.org) server written in Erlang. Can run with
[Cowboy](https://github.com/extend/cowboy) http server. SockJS-erlang
is in core web-framework agnostic (up to version
[v0.2.1](https://github.com/sockjs/sockjs-erlang/tree/v0.2.1 ) we also
supported
[Misultin](https://github.com/ostinelli/misultin)). SockJS-erlang is
compatible with
[SockJS client version 0.3](http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html). See
https://github.com/sockjs/sockjs-client for more information on
SockJS.


Show me the code!
-----------------

A simplistic echo SockJS server using Cowboy may look more or less
like this:

```erlang
main(_) ->
    ok = application:start(xmerl),
    ok = application:start(sockjs),
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(cowboy),

    SockjsState = sockjs_handler:init_state(
                    <<"/echo">>, fun service_echo/3, state, []),

    Routes = [{'_',  [{<<"/echo/[...]">>,
                       sockjs_cowboy_handler, SockjsState}]}],
    Dispatch = cowboy_router:compile(Routes),

    cowboy:start_http(cowboy_test_http_listener, 100,
                      [{port, 8081}],
                      [{env, [{dispatch, Dispatch}]}]),
    receive
        _ -> ok
    end.

service_echo(_Conn, init, state)          -> {ok, state};
service_echo(Conn, {recv, Data}, state)   -> Conn:send(Data); % or: sockjs:send(Data, Conn)
service_echo(_Conn, {info, _Info}, state) -> {ok, state};
service_echo(_Conn, closed, state)        -> {ok, state}.
```

Dig into the `examples` directory to get working code:

  * https://github.com/sockjs/sockjs-erlang/examples/cowboy_echo.erl



SockJS-erlang API
-----------------

Except for the web framework-specific API's, SockJS-erlang is rather
simple. It has just a couple of methods:

 * **sockjs_handler:init_state(Prefix, Callback, State, Options) -> service()**

    Initializes the state of a SockJS service (ie: a thing you can
    access from the browser, it has an url and a code on the server
    side). `prefix` is a binary that must exacty match the url prefix
    of the service, for example, if service will be listening on
    '/echo', this parameter must be set to `<<"/echo">>`. `callback`
    function will be called when a new SockJS connection is
    established, data received or a connection is closed. The value of
    `state` will be passed to the callback and preserved if returned
    value has changed. Options is a proplist that can contain
    following tuples:

     * `{sockjs_url, string()}` - Transports which don't support
       cross-domain communication natively ('eventsource' to name one)
       use an iframe trick.  A simple page is served from the SockJS
       server (using its foreign domain) and is placed in an invisible
       iframe. Code run from this iframe doesn't need to worry about
       cross-domain issues, as it's being run from domain local to the
       SockJS server. This iframe also does need to load SockJS
       javascript client library, and this option lets you specify its
       url (if you're unsure, point it to <a
       href="http://cdn.sockjs.org/sockjs-0.2.min.js"> the latest
       minified SockJS client release</a>, this is the default).
     * `{websocket, boolean()}` - are native websockets enabled? This
       can be usefull when your loadbalancer doesn't support them.
     * `{cookie_needed, boolean()}` - is your load balancer relying on
       cookies to get sticky sessions working?
     * `{cookie_name, string()}` - The name of the cookie
       (defaults to "JSESSIONID")
     * `{cookie_value, string()}` - The value of the cookie
       (defaults to "dummy")
     * `{heartbeat_delay, integer()}` - how often to send heartbeat
       packets (in ms).
     * `{disconnect_delay, integer()}` - how long to hold session state
       after the client was last connected (in ms).
     * `{response_limit, integer()}` - the maximum size of a single
       http streaming response (in bytes).
     * `{logger, fun/3}` - a function called on every request, used
       to print request to the logs (or on the screen by default).

    For more explanation, please do take a look at
    [SockJS-node readme](https://github.com/sockjs/sockjs-node/blob/master/README.md).

 * **Connection:send(Payload) -> ok**
 * **sockjs:send(Payload, Connection) -> ok**

     Send data over an active SockJS connection. Payload should be of
     iodata() type. Messages sent after connection gets closed will be
     lost.

 * **Connection:close(Code, Reason) -> ok**
 * **sockjs:close(Code, Reason, Connection) -> ok**
 * **sockjs:close(Connection) -> ok**

     Close an active SockJS connection with code and reason. If code
     and reason are skipped, the defaults are used.

 * **Connection:info() -> proplist()**
 * **sockjs:info(Connection) -> proplist()**

     Sometimes you may want to know more about the underlying
     connection. This method returns a proplist with few attributes
     extracted from the first HTTP/websocket request that was coming
     to this connection. You should see:

       * peername - ip address and port of the remote host
       * sockname - ip address and port of the local endpoint
       * path - the path used by the request that started the connection
       * headers - a set of headers extracted from the request that
         may be handy (don't expect to retrieve Cookie header).


The framework-specific calls are more problematic. Instead of trying
to explain how to use them, please take a look at the examples.

 * **type(req() :: {cowboy, request()})**
 * **sockjs_handler:handle_req(service(), req()) -> req()**
 * **sockjs_handler:handle_ws(service(), req()) -> req()**


What's news in this fork?
-------------------------

### API for multiplexing

 * **sockjs:send(Payload, Channel) -> ok**

     Send data over a channel. Payload should be of iodata() type. Messages
     sent after connection gets closed will be lost.

 * **sockjs:close(Code, Reason, Channel) -> ok**
 * **sockjs:close(Channel) -> ok**

     Close a channel with code and reason. If code and reason are skipped,
     the defaults are used. But actually, code and reason are not sent to
     client.

 * **sockjs:info(Channel) -> proplist()**

     Sometimes you may want to know more about the underlying
     connection. This method returns a proplist with few attributes
     extracted from the first HTTP/websocket request that was coming
     to this connection. You should see:

       * peername - ip address and port of the remote host
       * sockname - ip address and port of the local endpoint
       * path - the path used by the request that started the connection
       * headers - a set of headers extracted from the request that
         may be handy (don't expect to retrieve Cookie header).

 * **sockjs:to_session(Channel) -> conn()**

     Convert a channel to connection.

 * **sockjs:to_channel(Conn, Topic) -> channel()**

     Convert a connection to channel with specific topic (channel name).

 * **sockjs_multiplex:init_state(Services, {AuthenCallback, Options})**

     Sometimes you don't want client access directly your channel
     services and you want to do something with client first. If you
     use authentication callback, you can decide when allow client to
     use your services. It's quite simple, see [example](https://github.com/trubavuong/sockjs-erlang/blob/master/examples/multiplex/cowboy_multiplex_authen_callback.erl).
     Valid authentication callback options:

       * `{state, list()}` - initial state of authentication callback

     If you do not use multiplexing, you can also implement this
     mechanism, see [example](https://github.com/trubavuong/sockjs-erlang/blob/master/examples/cowboy_echo_authen_callback.erl).

 * **sockjs_multiplex:init_state(Services)**

     Initialize state without authentication callback.


### Other libraries

 * Use [jiffy](https://github.com/davisp/jiffy) to encode/decode JSON.
   I use it for better performance, [see benchmark from kivikakk.ee](https://kivikakk.ee/2013/05/20/erlang_is_slow.html).
 * Use the latest version of [Cowboy](https://github.com/extend/cowboy) (0.10.0).


Stability
---------

SockJS-erlang is quite new, but should be reasonably stable. Cowboy is passes all the
[SockJS-protocol tests](https://github.com/sockjs/sockjs-protocol).

Deployment and load balancing
-----------------------------

SockJS servers should work well behind many load balancer setups, but
it sometimes requres some additional twaks.  For more details, please
do take a look at the 'Deployment' section in
[SockJS-node readme](https://github.com/sockjs/sockjs-node/blob/master/README.md).


Development and testing
-----------------------

Use rebar3.

### SockJS-protocol Python tests

Once test_server is listening on `http://localhost:8081` you may test it
using SockJS-protocol:

    cd sockjs-protocol
    make test_deps
    ./venv/bin/python sockjs-protocol-dev.py

For details see
[SockJS-protocol README](https://github.com/sockjs/sockjs-protocol#readme).

### SockJS-client QUnit tests

You need to start a second web server (by default listening on 8080)
that is serving various static html and javascript files:

    cd sockjs-client
    make test

At that point you should have two web servers running: sockjs-erlang on
8081 and sockjs-client on 8080. When you open the browser on
[http://localhost:8080/](http://localhost:8080/) you should be able
run the QUnit tests against your sockjs-node server.

For details see
[SockJS-client README](https://github.com/sockjs/sockjs-client#readme).

Additionally, if you're doing more serious development consider using
`make serve`, which will automatically the server when you modify the
source code.
