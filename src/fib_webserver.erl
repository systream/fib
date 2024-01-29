%%%-------------------------------------------------------------------
%%% @author tihi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fib_webserver).

-define(WEBSERVER, rest_server).
-include("fib.hrl").

%% API
-export([start/0, stop/0]).

-spec start() -> ok.
start() ->
  Handlers = [fib_rest_handler, fib_seq_rest_handler,
              fib_blacklist_rest_handler,
              cowboy_swagger_handler],
  Dispatch = dispatch_rules(Handlers),
  {ok, Port} = application:get_env(?APP_NAME, port),
  {ok, _} = cowboy:start_clear(?WEBSERVER,
                               [{port, Port}],
                               #{env => #{dispatch => Dispatch},
                                 compress => true
                               }),
  ok.

-spec stop() -> ok.
stop() ->
  ok = cowboy:stop_listener(?WEBSERVER).

-spec dispatch_rules([module()]) -> cowboy_router:dispatch_rules().
dispatch_rules(Handlers) ->
  Trails = trails:trails(Handlers),
  trails:store(Trails),
  trails:single_host_compile(Trails).
