%%%-------------------------------------------------------------------
%% @doc aoc_erl public API
%% @end
%%%-------------------------------------------------------------------

-module(aoc_erl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aoc_erl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
