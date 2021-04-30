-module(sockjs_session_sup).

-behaviour(supervisor).

-export([start_child/3, start_link/0]).

-export([init/1]).

%% --------------------------------------------------------------------------

-spec start_link() ->
    ignore
    | {ok, pid()}
    | {error, any()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {{simple_one_for_one, 10000, 1}, [
            {undefined, {sockjs_session, start_link, []}, temporary, 5000, worker, [sockjs_session]}
        ]}}.

start_child(SessionId, Service, Info) ->
    supervisor:start_child(
        ?MODULE,
        [SessionId, Service, Info]
    ).
