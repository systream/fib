%%%-------------------------------------------------------------------
%%% @author tihi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fib_seq_rest_handler).

-behaviour(cowboy_rest).
-behaviour(trails_handler).

-include("fib.hrl").

-export([init/2,
         malformed_request/2,
         content_types_provided/2,
         allowed_methods/2]).

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
           true =:= fib_validator:is_positive(N),
  % @TODO check limit, offset
  {not Result, Req, State}.

-spec trails() -> [trails:trail()].
trails() ->
  Metadata =
    #{get =>
      #{tags => [<<"Fibonacci sequenes">>],
      description => <<"Return list of number from the Fibonacci sequene">>,
      parameters =>
        [#{name => <<"n">>,
          description => <<"Sequene number">>,
          in => <<"path">>,
          required => true,
          schema => #{type => integer,
                      minimum => 1,
                      example => 3}},
          #{name => <<"offset">>,
            description => <<"Pagination offset">>,
            in => <<"query">>,
            required => false,
            schema => #{type => integer,
                        minimum => 0,
                        example => 0}},
          #{name => <<"limit">>,
            description => <<"Pagination limit">>,
            in => <<"query">>,
            required => false,
            schema => #{type => integer,
                        minimum => 1,
                        example => 100}}
          ],
      responses =>
        #{<<"200">> =>
           #{description => <<"Fibonacci number">>,
             content => #{'application/json' => #{schema => #{type => array,
                                                              items => #{type => integer}}}}
           },
          <<"400">> => #{description => <<"Bad request">>},
          <<"500">> => #{description => <<"Internal error">>},
          <<"503">> => #{description => <<"Temporary error">>}
        }
      }
    },
  [trails:trail("/calculations/fibonacci-sequence/:n", ?MODULE, [], Metadata)].

-spec calculate(cowboy_req:req(), map()) -> {binary(), cowboy_req:req(), map()}.
calculate(Req, State) ->
  N = binary_to_integer(cowboy_req:binding(n, Req)),
  QsVals = cowboy_req:parse_qs(Req),
  Offset = page_offset(QsVals),
  Limit = page_limit(QsVals),
  Result = fib_calculator:calculate_seq(Offset, Limit, N),
  {jsx:encode(Result), Req, State}.

-spec page_limit(proplists:proplist()) -> integer().
page_limit(QsVals) ->
  case lists:keyfind(<<"limit">>, 1, QsVals) of
    false ->
      {ok, Default} = application:get_env(?APP_NAME, default_page_size),
      Default;
    {_, QueryParamLimit} ->
      binary_to_integer(QueryParamLimit)
  end.

-spec page_offset(proplists:proplist()) -> integer().
page_offset(QsVals) ->
  case lists:keyfind(<<"offset">>, 1, QsVals) of
    false -> 0;
    {_, QueryParamOffset} -> binary_to_integer(QueryParamOffset)
  end.
