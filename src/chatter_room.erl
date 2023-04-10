-module(chatter_room).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([
    join/2,
    leave/2,
    send_message/3
]).

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, ?MODULE).
-define(MSG_TYPE_NEW, <<"new_message">>).
-define(MSG_TYPE_SERVICE, <<"service_message">>).

-record(state, {pids, users, quotes}).
%%% API

-spec join(Room, Username) -> ok when
    Room :: atom(),
    Username :: binary().
join(Room, Username) ->
    {ok, _Pid} = create_room_if_not_exists(Room),
    gen_server:call(Room, {join, Username}).

-spec leave(Room, Username) -> ok when
    Room :: atom(),
    Username :: binary().
leave(Room, Username) ->
    gen_server:call(Room, {remove_user, Username}).

-spec send_message(Room, Username, Msg) -> ok when
    Room :: atom(),
    Username :: binary(),
    Msg :: binary().
send_message(Room, Username, Msg) ->
    gen_server:call(Room, {send_message, Username, Msg}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Room) ->
    gen_server:start_link({local, Room}, ?MODULE, [], []).

init([]) ->
    set_random_timer(),
    {ok, Quotes} = read_bot_quotes(),
    {ok, #state{pids = [], users = [], quotes = Quotes}}.

handle_call({join, Username}, {Pid, _} = _From, State) ->
    #state{pids = Receivers, users = Users} = NewState = do_join_user(Username, Pid, State),
    Payload = <<Username/binary, " joined to the room">>,
    distribute(?MSG_TYPE_SERVICE, Payload, Receivers),
    distribute(?MSG_TYPE_SERVICE, list_users(Users), Receivers),
    {reply, ok, NewState};
handle_call({remove_user, Username}, {Pid, _} = _From, State) ->
    #state{pids = Receivers, users = Users} = NewState = do_remove_user(Username, Pid, State),
    Payload = <<Username/binary, " left the room">>,
    distribute(?MSG_TYPE_SERVICE, Payload, Receivers),
    distribute(?MSG_TYPE_SERVICE, list_users(Users), Receivers),
    {reply, ok, NewState};
handle_call({send_message, Username, Msg}, _From, #state{pids = Receivers} = State) ->
    Payload = <<Username/binary, ": ", Msg/binary>>,
    distribute(?MSG_TYPE_NEW, Payload, Receivers),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, wake_up_bot}, #state{pids = Receivers, quotes = Quotes} = State) ->
    Num = rand:uniform(erlang:length(Quotes)),
    Msg = lists:nth(Num, Quotes),
    Payload = <<"___CHAT_BOT___: ", Msg/binary>>,
    distribute(?MSG_TYPE_NEW, Payload, Receivers),
    set_random_timer(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_room_if_not_exists(Room) ->
    create_room_if_not_exists(whereis(Room), Room).

create_room_if_not_exists(undefined, Room) ->
    supervisor:start_child(chatter_room_sup, [Room]);
create_room_if_not_exists(Pid, _Room) ->
    {ok, Pid}.

do_join_user(Username, Pid, #state{pids = Pids, users = Users} = State) ->
    State#state{pids = [Pid | Pids], users = [Username | Users]}.

do_remove_user(Username, Pid, #state{pids = Pids, users = Users} = State) ->
    State#state{pids = lists:delete(Pid, Pids), users = lists:delete(Username, Users)}.

distribute(Type, Payload, Receivers) ->
    lists:foreach(fun(Pid) -> Pid ! {Type, Payload} end, Receivers).

set_random_timer() ->
    Timeout = rand:uniform(55) * 1000,
    erlang:start_timer(Timeout, self(), wake_up_bot).

read_bot_quotes() ->
    File = filename:join([code:priv_dir(chatter), "dawn.txt"]),
    {ok, Text} = file:read_file(File),
    Lines = lists:filter(fun(Str) -> Str =/= <<>> end, binary:split(Text, <<"\r\n">>, [global])),
    {ok, Lines}.

list_users(Users) ->
    lists:foldl(fun(User, Acc) -> <<Acc/binary, User/binary, " ">> end, <<"List users: ">>, Users).