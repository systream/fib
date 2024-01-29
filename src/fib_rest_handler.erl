%%%-------------------------------------------------------------------
%%% @author tihi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fib_rest_handler).

-behaviour(cowboy_rest).
-behaviour(trails_handler).

-export([init/2,
         content_types_provided/2,
         malformed_request/2,
         allowed_methods/2, forbidden/2]).

-export([calculate/2]).
-export([trails/0]).

-spec init(cowboy_req:req(), any()) ->
  {cowboy_rest, cowboy_req:req(), map()}.
init(Req, _Opts) ->
  {cowboy_rest, Req, #{}}.

-spec allowed_methods(cowboy_req:req(), map()) ->
  {[binary()], cowboy_req:req(), map()}.
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), map()) ->
  {[{binary(), atom()}], cowboy_req:req(), map()}.
content_types_provided(Req, State) ->
  {[{<<"application/json">>, calculate}], Req, State}.

-spec malformed_request(Req, State) -> {boolean(), Req, State}
  when Req :: cowboy_req:req(), State :: any().
malformed_request(Req, State) ->
  N = cowboy_req:binding(n, Req),
  Result = true =:= fib_validator:is_number(N) andalso
           true =:= fib_validator:is_non_negative(N),
  {not Result, Req, State}.

-spec forbidden(Req, State) -> {boolean(), Req, State}
  when Req :: cowboy_req:req(), State :: any().
forbidden(Req, State) ->
  N = binary_to_integer(cowboy_req:binding(n, Req)),
  Result = fib_calculator:calculate(N),
  {fib_blacklist_handler:is_blacklisted(Result), Req, State#{result => Result}}.

-spec trails() -> [trails:trail()].
trails() ->
  Metadata =
    #{get =>
      #{tags => [<<"Fibonacci">>],
      description => <<"Return the value from the Fibonacci sequene for a given number">>,
      parameters =>
        [#{name => <<"n">>,
          description => <<"Sequene number">>,
          in => <<"path">>,
          required => true,
          schema => #{type => integer,
                      minimum => 0,
                      example => 3}}],
      responses =>
        #{<<"200">> =>
           #{description => <<"Fibonacci number">>,
             content => #{'application/json' => #{schema => #{type => integer}}}
           },
          <<"400">> => #{description => <<"Bad request">>},
          <<"403">> => #{description => <<"Number disabled">>},
          <<"500">> => #{description => <<"Internal error">>},
          <<"503">> => #{description => <<"Temporary error">>}
        }
      }
    },
  [trails:trail("/calculations/fibonacci/:n", ?MODULE, [], Metadata)].

-spec calculate(cowboy_req:req(), map()) -> {binary(), cowboy_req:req(), map()}.
calculate(Req, #{result := Result} = State) ->
  {integer_to_binary(Result), Req, State}.
