-module(chatter_middleware).
-behaviour(cowboy_middleware).

%% API
-export([
    execute/2
]).

-type x_session_data() :: #{
    x_user => binary(),
    x_room => binary(),
    x_session_id => binary()
}.

-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req} when
    Req :: cowboy_req:req(),
    Env :: #{atom() => any()}.
execute(#{path := Path} = Req, Env) when Path =:= <<"/login">> ->
    {ok, Req, Env};
execute(Req, Env) ->
    case check_cookies(Req) of
        {ok, SessionData} ->
            {ok, maps:merge(Req, SessionData), Env};
        {error, not_identified} ->
            Req1 = cowboy_req:reply(302, #{ <<"location">> => <<"/login">> }, Req),
            {stop, Req1}
    end.

%% Internal functions

-spec check_cookies(Req) -> {ok, x_session_data()} | {error, atom()} when
    Req :: cowboy_req:req().
check_cookies(Req) ->
    case cowboy_req:match_cookies([{x_user, [], undefined}, {x_room, [], undefined}, {x_session_id, [], undefined}], Req) of
        #{
            x_user := User,
            x_room := Room,
            x_session_id := SessionId
        } when User =:= undefined; Room =:= undefined; SessionId =:= undefined ->
            {error, not_identified};
        #{
            x_user := _User,
            x_room := Room,
            x_session_id := SessionId
        } = SessionData ->
            case chatter_identity:is_valid_session(SessionId, Room) of
                true -> {ok, SessionData};
                false -> {error, not_identified}
            end;
        _ ->
            {error, not_identified}
    end.
