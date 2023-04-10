-module(chatter_cth).

%% API
-export([
    init/2,
    terminate/1,
    pre_init_per_suite/3
]).

-include_lib("common_test/include/ct.hrl").

init(_Id, State) ->
    start_chatter(State).

pre_init_per_suite(_SuiteName, Config, State) ->
    {Config ++ State, State}.

terminate(_State) -> ok.

%% Internal functions

start_chatter(State) ->
    application:load(chatter),

    Port = get_free_port(),
    application:set_env(chatter, port, Port),
    application:set_env(chatter, ip, {127,0,0,1}),

    application:ensure_all_started(gun),

    {ok, _} = application:ensure_all_started(chatter),
    State.

get_free_port() ->
    {ok, Listen} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(Listen),
    ok = gen_tcp:close(Listen),
    Port.