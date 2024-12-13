-module(L6_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, L6_sup}, L6_sup, []).

init([]) ->
    %% Define worker and restart strategy
    {ok, {{one_for_one, 5, 10}, [
        {L6_cache, {L6_cache, start_link, []}, permanent, 5000, worker, [L6_cache]}
    ]}}.
