%%%-------------------------------------------------------------------
%%% @author losto
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(chatter_room_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
        intensity => 3,
        period => 1000},
    ChildSpecs = [
        #{
            id => chatter_room,
            start => {chatter_room, start_link, []},
            restart => temporary
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
