-module(chatter_test_SUITE).

-export([
    init_per_suite/1,
    end_per_suite/1,
    all/0
]).

-export([
    websocket_test/1,
    identity_flow_test/1
]).

-include_lib("eunit/include/eunit.hrl").

-define(IP, {127,0,0,1}).

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() -> [
    identity_flow_test,
    websocket_test
].

identity_flow_test(_Config) ->
    {ok, Port} = application:get_env(chatter, port),
    ConnPid = chatter_ct:connect(?IP, Port, #{cookie_store => gun_cookies_list:init(#{})}),

    %% not identified request
    {ok, {RespCode1, RespHeaders1, _}} = chatter_ct:get(ConnPid, <<"/">>, []),
    {<<"location">>, Redirect1} = lists:keyfind(<<"location">>, 1, RespHeaders1),

    ?assertEqual(302, RespCode1),
    ?assertEqual(<<"/login">>, Redirect1),

    %% get static form
    {ok, {RespCode2, _RespHeaders2, _}} = chatter_ct:get(ConnPid, Redirect1, []),

    ?assertEqual(200, RespCode2),

    %% identification by form
    PostData = <<"username=test_user&room=common">>,
    PostHeaders = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    {ok, {RespCode3, RespHeaders3, _}} = chatter_ct:post(ConnPid, Redirect1, PostHeaders, PostData),

    {<<"location">>, Redirect3} = lists:keyfind(<<"location">>, 1, RespHeaders3),

    ?assertEqual(302, RespCode3),
    ?assertEqual(<<"/">>, Redirect3),

    % check cookies
    #{room := Room, user := User} = lists:foldl(fun
        (<<"x_room=", Tail/binary>>, Acc) ->
            [R | _] = binary:split(Tail, <<";">>),
            Acc#{room => R};
        (<<"x_user=", Tail/binary>>, Acc) ->
            [U | _] = binary:split(Tail, <<";">>),
            Acc#{user => U};
        (_, Acc) -> Acc
    end, #{room => undefined, user => undefined}, proplists:get_all_values(<<"set-cookie">>, RespHeaders3)),

    ?assertEqual(<<"test_user">>, User),
    ?assertEqual(<<"common">>, Room),

    %% identified request
    {ok, {RespCode4, _RespHeaders4, _}} = chatter_ct:get(ConnPid, <<"/">>, []),

    ?assertEqual(200, RespCode4),

    chatter_ct:disconnect(ConnPid).

websocket_test(_Config) ->
    %% identification
    {ok, Port} = application:get_env(chatter, port),
    ConnPid = chatter_ct:connect(?IP, Port, #{cookie_store => gun_cookies_list:init(#{})}),
    PostData = <<"username=test_user2&room=test2">>,
    PostHeaders = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    {ok, {302, _, _}} = chatter_ct:post(ConnPid, <<"/login">>, PostHeaders, PostData),
    {ok, StreamRef} = chatter_ct:ws_upgrade(ConnPid, <<"/websocket">>, []),

    %% check service message join
    {ok, JoinMsg} = chatter_ct:ws_recv(ConnPid),
    ?assertEqual(#{<<"data">> => <<"test_user2 joined to the room">>, <<"type">> => <<"service_message">>}, JoinMsg),

    %% check service message list_users
    {ok, UsersMsg} = chatter_ct:ws_recv(ConnPid),
    ?assertEqual(#{<<"data">> => <<"List users: test_user2 ">>, <<"type">> => <<"service_message">>}, UsersMsg),

    %% send message
    Msg = #{<<"type">> => <<"send_message">>, <<"data">> => <<"Hi there!">>},
    chatter_ct:ws_send(ConnPid, StreamRef, Msg),

    %% receive from server
    {ok, RcvMsg} = chatter_ct:ws_recv(ConnPid),
    ?assertEqual(#{<<"data">> => <<"test_user2: Hi there!">>, <<"type">> => <<"new_message">>}, RcvMsg),

    chatter_ct:disconnect(ConnPid).

