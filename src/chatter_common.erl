-module(chatter_common).

%% API
-export([
    www_dir/0,
    parse_request/2
]).

-spec www_dir() -> file:filename_all().
www_dir() ->
    case application:get_env(chatter, www_dir) of
        {ok, Path} -> Path;
        undefined ->
            PrivDir = code:priv_dir(chatter),
            filename:join([PrivDir, "www"])
    end.

-spec parse_request(Data, Model) -> {ok, map()} | {error, binary()} when
    Data :: map(),
    Model :: map().
parse_request(Data, Model) ->
    try parse_request_(Data, Model) of
        {ok, _} = Ok -> Ok
    catch
        _:Error -> Error
    end.

%% Internal functions

parse_request_(Data, Model) ->
    maps:fold(fun(Key, #{type := Type, required := Req}, Acc) ->
        check_field(Key, maps:get(Key, Data, undefined), Req, Type, Acc)
    end, {ok, #{}}, Model).

check_field(_, undefined, true, _, _) -> error({error, <<"Missing required param">>});
check_field(_, <<>>, true, binary, _) -> error({error, <<"Missing required param">>});
check_field(_, undefined, false, _, Acc) -> Acc;
check_field(Key, Value, _, Type, Acc) -> check_type(Key, Value, Type, Acc).

check_type(Key, Value, binary, {ok,Acc}) when is_binary(Value) -> {ok, Acc#{Key => Value}};
check_type(_Key, _Value, binary, {ok,_Acc}) -> error({error, <<"Bad type param, expected binary">>}).