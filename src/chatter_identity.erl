-module(chatter_identity).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([
    start_session/2,
    drop_session/1,
    is_valid_session/2
]).

-include_lib("stdlib/include/qlc.hrl").

-define(SERVER, ?MODULE).
-define(SESSION_TBL, session_tbl).

-record(chatter_identity_state, {}).

%%% API

-spec start_session(Username, Room) -> {ok, binary()} | {error, binary()} when
    Username :: binary(),
    Room :: binary().
start_session(Username, Room) ->
    gen_server:call(?MODULE, {start_session, Username, Room}).

-spec drop_session(SessionId) -> ok when
    SessionId :: binary().
drop_session(SessionId) ->
    gen_server:call(?MODULE, {drop_session, SessionId}).

-spec is_valid_session(SessionId, Room) -> boolean() when
    SessionId :: binary(),
    Room :: binary().
is_valid_session(SessionId, Room) ->
    gen_server:call(?MODULE, {is_valid_session, SessionId, Room}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?SESSION_TBL, [named_table]),
    {ok, #chatter_identity_state{}}.

handle_call({start_session, Username, Room}, _From, State = #chatter_identity_state{}) ->
    Reply = do_start_session(Username, Room),
    {reply, Reply, State};
handle_call({drop_session, SessionId}, _From, State = #chatter_identity_state{}) ->
    do_drop_session(SessionId),
    {reply, ok, State};
handle_call({is_valid_session, SessionId, Room}, _From, State = #chatter_identity_state{}) ->
    Reply = check_session(SessionId, Room),
    {reply, Reply, State};
handle_call(_Request, _From, State = #chatter_identity_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #chatter_identity_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #chatter_identity_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #chatter_identity_state{}) ->
    ok.

code_change(_OldVsn, State = #chatter_identity_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_start_session(Username, Room) ->
    Q = qlc:q([User || {_SessionId, User, _Room} <- ets:table(?SESSION_TBL), User =:= Username]),
    case qlc:e(Q) of
        [] ->
            SessionId = unicode:characters_to_binary(uuid:uuid_to_string(uuid:get_v4())),
            ets:insert(?SESSION_TBL, {SessionId, Username, Room}),
            {ok, SessionId};
        _ ->
            {error, <<"User exists">>}
    end.

do_drop_session(SessionId) ->
    ets:delete(?SESSION_TBL, SessionId).

check_session(SessionId, Room) ->
    case ets:lookup(?SESSION_TBL, SessionId) of
        [{SessionId, _, Room}] -> true;
        _ -> false
    end.