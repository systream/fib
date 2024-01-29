%%%-------------------------------------------------------------------
%%% @author tihi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fib_blacklist_rest_handler).

-behaviour(cowboy_rest).
-behaviour(trails_handler).

-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         malformed_request/2,
         allowed_methods/2, delete_resource/2]).

-export([add/2]).
-export([trails/0]).

-spec init(cowboy_req:req(), any()) ->
  {cowboy_rest, cowboy_req:req(), map()}.
init(Req, _Opts) ->
  {cowboy_rest, Req, #{}}.

-spec allowed_methods(cowboy_req:req(), map()) ->
  {[binary()], cowboy_req:req(), map()}.
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"DELETE">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), map()) ->
  {proplists:proplist(), cowboy_req:req(), map()}.
content_types_provided(#{bindings := #{n := _}} = Req, State) ->
  {[{<<"application/json">>, remove}], Req, State};
content_types_provided(Req, State) ->
  {[{<<"application/json">>, add}], Req, State}.

-spec content_types_accepted(cowboy_req:req(), map()) ->
  {proplists:proplist(), cowboy_req:req(), map()}.
content_types_accepted(Req, State) ->
  {[{<<"application/json">>, add}], Req, State}.

-spec malformed_request(Req, State) -> {boolean(), Req, State}
  when Req :: cowboy_req:req(), State :: any().
malformed_request(#{bindings := #{n := Number}} = Req, State) ->
  ValidationResult = true =:= fib_validator:is_number(Number) andalso
                     true =:= fib_validator:is_non_negative(Number),
  {not ValidationResult, Req, State};
malformed_request(Req, State) ->
  {ok, Data, Req1} = cowboy_req:read_body(Req),
  {Result, NewState} =
    case jsx:is_json(Data) of
      true ->
        EncodedData = jsx:decode(Data),
        ValidationResult = true =:= fib_validator:is_number(EncodedData) andalso
                           true =:= fib_validator:is_non_negative(EncodedData),
        {ValidationResult, State#{number => EncodedData}};
      _ ->
        {false, State}
    end,
  {not Result, Req1, NewState}.

-spec trails() -> [trails:trail()].
trails() ->
  RootMetadata =
    #{post =>
      #{tags => [<<"Blacklist">>],
        description => <<"Add blacklist item">>,
        parameters => [
          #{name => <<"number">>,
            description => <<"Blacklisted number">>,
            in => <<"body">>,
            required => true,
            schema => #{type => integer,
                        minimum => <<"0">>}}
        ],
        responses =>
        #{<<"204">> => #{description => <<"Successfully added to blacklist">>},
          <<"400">> => #{description => <<"Bad request">>},
          <<"500">> => #{description => <<"Internal error">>},
          <<"503">> => #{description => <<"Temporary error">>}
        }
      }
    },
  Metadata =
    #{delete =>
    #{tags => [<<"Blacklist">>],
      description => <<"Remove blacklist item">>,
      parameters => [
        #{name => <<"n">>,
          description => <<"Blacklisted number">>,
          in => <<"path">>,
          required => true,
          schema => #{type => integer,
            minimum => 0,
            example => 100}}
      ],
      responses =>
      #{<<"204">> => #{description => <<"Successfully removed from blacklist">>},
        <<"400">> => #{description => <<"Bad request">>},
        <<"500">> => #{description => <<"Internal error">>},
        <<"503">> => #{description => <<"Temporary error">>}
      }
    }
    },
  [trails:trail("/blacklists", ?MODULE, [], RootMetadata),
   trails:trail("/blacklists/:n", ?MODULE, [], Metadata)].

-spec add(cowboy_req:req(), map()) -> {boolean(), cowboy_req:req(), map()}.
add(Req, #{number := Number} = State) ->
  % @TODO maybe we should check that is that number a possible fibonacci number or not.
  % You can save whatever you want, but a non fibonacci number is useless
  % maybe i have no time for that
  ok = fib_blacklist_handler:add(Number),
  {true, Req, State}.

-spec delete_resource(cowboy_req:req(), map()) ->
  {boolean(), cowboy_req:req(), map()}.
delete_resource(Req, State) ->
  Number = binary_to_integer(cowboy_req:binding(n, Req)),
  ok = fib_blacklist_handler:remove(Number),
  {true, Req, State}.
