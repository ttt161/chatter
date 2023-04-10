-module(chatter_login_handler).

-behaviour(cowboy_rest).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2
]).

-export([
    get_login_form/2,
    try_join_to_room/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% cowboy_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req, _Opts) ->
    {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"*">>, <<"*">>, []}, get_login_form}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, try_join_to_room}], Req, State}.

%%%%%%%%%%%%%%%%%%%%%
%%% application logic
%%%%%%%%%%%%%%%%%%%%%

-spec get_login_form(Req, State) -> {stop, cowboy_req:req(), any()} when
    Req :: cowboy_req:req(),
    State :: any().
get_login_form(Req, State) ->
    Params = [{msg, "Enter your name and chat-room"}],
    ReqR = static_reply(200, Params, Req),
    {stop, ReqR, State}.

-spec try_join_to_room(Req, State) -> {stop, cowboy_req:req(), any()} when
    Req :: cowboy_req:req(),
    State :: any().
try_join_to_room(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case chatter_common:parse_request(parse_form(Body), model()) of
        {ok, #{<<"username">> := Username, <<"room">> := Room}} ->
            case chatter_identity:start_session(Username, Room) of
                {ok, SessionId} ->
                    Req2 = cowboy_req:set_resp_cookie(<<"x_user">>, Username, Req1, #{http_only => true}),
                    Req3 = cowboy_req:set_resp_cookie(<<"x_room">>, Room, Req2, #{http_only => true}),
                    Req4 = cowboy_req:set_resp_cookie(<<"x_session_id">>, SessionId, Req3, #{http_only => true}),
                    {stop, cowboy_req:reply(302, #{<<"location">> => <<"/">>}, Req4), State};
                {error, Reason} ->
                    Params = [{msg, Reason}],
                    ReqR = static_reply(400, Params, Req1),
                    {stop, ReqR, State}
            end;
        {error, Reason} ->
            Params = [{msg, Reason}],
            ReqR = static_reply(400, Params, Req1),
            {stop, ReqR, State}
    end.

%% Internal functions

static_reply(HttpCode, Params, Req) ->
    {ok, Module} = erlydtl:compile(filename:join([chatter_common:www_dir(), "login.dtl"]), login),
    {ok, Body} = Module:render(Params),
    Headers = #{<<"content-type">> => <<"text/html">>},
    cowboy_req:reply(HttpCode, Headers, Body, Req).

model() ->
    #{
        <<"username">> => #{type => binary, required => true},
        <<"room">> => #{type => binary, required => true}
    }.

parse_form(Body) ->
    Pairs = binary:split(Body, <<"&">>, [global]),
    lists:foldl(fun(Pair, Acc) -> [Key, Value] = binary:split(Pair, <<"=">>), Acc#{Key => Value} end, #{}, Pairs).

