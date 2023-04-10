-module(chatter_ct).

%% API
-export([
    connect/2,
    connect/3,
    disconnect/1,
    get/3,
    post/4,
    get_response/2,
    ws_upgrade/3,
    ws_send/3,
    ws_recv/1
]).

connect(Host, Port) ->
    connect(Host, Port, #{}).

connect(Host, Port, Opts) ->
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    {ok, _} = gun:await_up(ConnPid),
    ConnPid.

disconnect(ConnPid) ->
    gun:close(ConnPid).

get(ConnPid, Path, Headers) ->
    StreamRef = gun:get(ConnPid, Path, Headers),
    get_response(ConnPid, StreamRef).

post(ConnPid, Path, Headers, Body) ->
    StreamRef = gun:post(ConnPid, Path, Headers, Body),
    get_response(ConnPid, StreamRef).

get_response(ConnPid, StreamRef) ->
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} ->
            {ok, {Status, Headers, <<>>}};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            {ok, {Status, Headers, Body}}
    end.

ws_upgrade(ConnPid, Path, Headers) ->
    WsUpgradeRef = gun:ws_upgrade(ConnPid, Path, Headers),
    receive
        {gun_upgrade, ConnPid, WsUpgradeRef, [<<"websocket">>], _} ->
            {ok, WsUpgradeRef}
    after 1000 ->
        {error, failed}
    end.

ws_send(ConnPid, StreamRef, Req) when is_map(Req) ->
    Encoded = jsx:encode(Req),
    gun:ws_send(ConnPid, StreamRef, {text, Encoded}).

ws_recv(ConnPid) ->
    receive
        {gun_ws, ConnPid, _, {text, Data}} ->
            Msg = jsx:decode(Data, [return_maps]),
            {ok, Msg}
    after 1000 ->
        {error, timeout}
    end.