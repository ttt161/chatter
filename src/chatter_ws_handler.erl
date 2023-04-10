-module(chatter_ws_handler).

-behaviour(cowboy_websocket).
-export([init/2]).

-export([
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-type session_data() :: #{
    user => binary(),
    room => atom(),
    session_id => binary()
}.

-spec init(Req, Params) -> {cowboy_websocket, cowboy_req:req(), session_data()} when
    Req :: cowboy_req:req(),
    Params :: any().
init(#{x_user := User, x_room := Room, x_session_id := SessionId} = Req, _Params) ->
    {cowboy_websocket, Req, #{user => User, room => erlang:binary_to_atom(Room), session_id => SessionId}}.

websocket_init(#{user := User, room := Room} = State) ->
    ok = chatter_room:join(Room, User),
    erlang:start_timer(15000, self(), ping),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    try jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := Type, <<"data">> := Data} ->
            do_handle(Type, Data, State)
    catch _Ex:_Er ->
        logger:error("~p: ~p", [_Ex, _Er])
    end,
    {ok, State};
websocket_handle(pong, State) ->
    erlang:start_timer(15000, self(), ping),
    {ok, State}.

websocket_info({timeout, _Ref, ping}, State) ->
    {reply, ping, State};
websocket_info({Type, Payload}, State) ->
    Data = #{type => Type, data => Payload},
    try jsx:encode(Data) of
        EncodedData ->
            {reply, {text, EncodedData}, State}
    catch _Ex:_Er ->
        logger:error("~p: ~p", [_Ex, _Er]),
        {ok, State}
    end.

terminate(_Reason, _Req,  #{user := User, room := Room, session_id := SessionId} =_State) ->
    chatter_room:leave(Room, User),
    chatter_identity:drop_session(SessionId),
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

%% Internal functions

do_handle(<<"send_message">>, Payload, #{user := User, room := Room}) ->
    chatter_room:send_message(Room, User, Payload).