%%%-------------------------------------------------------------------
%% @doc fib public API
%% @end
%%%-------------------------------------------------------------------

-module(fib_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fib_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
