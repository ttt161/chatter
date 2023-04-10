%%%-------------------------------------------------------------------
%% @doc chatter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chatter_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([
    start_listener/0
]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        % Identity service
        #{
            id => identity_srv,
            start => {chatter_identity, start_link, []}
        },
        % Room sup
        #{
            id => room_sup,
            start => {chatter_room_sup, start_link, []},
            type => supervisor
        },
        % Listener
        #{
            id => listener,
            start => {?MODULE, start_listener, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

start_listener() ->
    IP = application:get_env(chatter, ip, {127, 0, 0, 1}),
    Port = application:get_env(chatter, port, 8088),
    Timeout = application:get_env(chatter, connection_timeout, 60000),
    WWW = chatter_common:www_dir(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, filename:join(WWW, "index.html")}},
            {"/login", chatter_login_handler, #{}},
            {"/websocket", chatter_ws_handler, #{}},
            {"/[...]", cowboy_static, {dir, WWW}}
        ]}
    ]),
    Args = [
        {ip, IP},
        {port, Port}
    ],
    Opts = #{
        env => #{
            dispatch => Dispatch
        },
        request_timeout => Timeout,
        middlewares => [
            chatter_middleware,
            cowboy_router,
            cowboy_handler
        ]
    },
    {ok, _} = cowboy:start_clear(http_listener, Args, Opts).
