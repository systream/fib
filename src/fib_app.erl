%%%-------------------------------------------------------------------
%% @doc fib public API
%% @end
%%%-------------------------------------------------------------------

-module(fib_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(StartType :: application:start_type(), StartArgs :: term()) ->
  {'ok', pid()}.
start(_StartType, _StartArgs) ->
  {ok, _} = Result = fib_sup:start_link(),
  ok = fib_webserver:start(),
  Result.

-spec stop(term()) -> ok.
stop(_State) ->
  ok = fib_webserver:stop(),
  ok.

%% internal functions
