%%%-------------------------------------------------------------------
%% @doc registry_bench public API
%% @end
%%%-------------------------------------------------------------------

-module(registry_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    registry_bench_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
