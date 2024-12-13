-module(L6_cache).
-behaviour(gen_server).

-export([start_link/0, create/1, insert/4, lookup/2, delete_obsolete/1]).
-export([init/1, handle_call/3, handle_info/2]).

-record(cache_entry, {value, timestamp, expiry}).

start_link() ->
    gen_server:start_link({local, L6_cache}, L6_cache, [], []).

init([]) ->
    {ok, #{}}, 

handle_call({create, TableName}, _From, State) ->
    case ets:info(TableName) of
        undefined -> 
            ets:new(TableName, [set, public, named_table, {keypos, 2}]),
            {reply, {ok, self()}, State};
        _ -> 
            {reply, {error, already_exists}, State}
    end;

handle_call({insert, TableName, Key, Value}, _From, State) ->
    ts = calendar:local_time(),
    ets:insert(TableName, {Key, #cache_entry{value = Value, timestamp = ts, expiry = undefined}}),
    {reply, ok, State};

handle_call({insert, TableName, Key, Value, Expiry}, _From, State) ->
    ts = calendar:local_time(),
    ets:insert(TableName, {Key, #cache_entry{value = Value, timestamp = ts, expiry = Expiry}}),
    {reply, ok, State};

handle_call({lookup, TableName, Key}, _From, State) ->
    case ets:lookup(TableName, Key) of
        [{_Key, #cache_entry{value = Value, timestamp = Timestamp, expiry = Expiry}}] ->
            %% Check expiry
            case Expiry of
                undefined -> {reply, Value, State};
                _ when calendar:now_to_universal_time(Timestamp) + Expiry > calendar:now_to_universal_time(calendar:local_time()) ->
                    {reply, Value, State};
                _ -> 
                    ets:delete(TableName, Key),
                    {reply, undefined, State}
            end;
        [] -> 
            {reply, undefined, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

delete_obsolete(TableName) ->
    now = calendar:local_time(),
    ets:foldl(fun(Key, {_Entry, Value}, Acc) ->
                        case Value of
                            #cache_entry{expiry = Expiry, timestamp = Timestamp} when
                                (Expiry /= undefined) andalso
                                (calendar:now_to_universal_time(Timestamp) + Expiry < calendar:now_to_universal_time(now)) ->
                                    ets:delete(TableName, Key),
                                    Acc;
                            _ -> Acc
                        end
                    end, [], TableName).
