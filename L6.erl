-module(L6).
-behaviour(application).

-export([start/0, stop/0, create/1, insert/3, insert/4, lookup/2]).

start() ->
    L6_sup:start_link().

stop() ->
    ok.

create(TableName) ->
    case gen_server:call(L6_cache, {create, TableName}) of
        {ok, _Pid} -> ok;
        {error, _} -> {error, already_exists}
    end.

insert(TableName, Key, Value) ->
    gen_server:call(L6_cache, {insert, TableName, Key, Value}).

insert(TableName, Key, Value, Expiry) ->
    gen_server:call(L6_cache, {insert, TableName, Key, Value, Expiry}).

lookup(TableName, Key) ->
    gen_server:call(L6_cache, {lookup, TableName, Key}).
