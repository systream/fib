%%%-------------------------------------------------------------------
%%% @author tihi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fib_validator).

%% API
-export([is_number/1, is_positive/1, is_non_negative/1]).

-spec is_number(binary() | integer()) -> boolean().
is_number(Bin) when is_binary(Bin) ->
  case [ X || <<X/integer>> <= Bin, X < $0 orelse X > $9 ] of
    [] -> true;
    _ -> false
  end;
is_number(Int) when is_integer(Int) ->
  true.

-spec is_positive(binary() | integer()) -> boolean().
is_positive(Num) when is_integer(Num) ->
  Num > 0;
is_positive(Bin) when is_binary(Bin) ->
  is_positive(binary_to_integer(Bin)).

-spec is_non_negative(binary() | integer()) -> boolean().
is_non_negative(Num) when is_integer(Num) ->
  Num >= 0;
is_non_negative(Bin) when is_binary(Bin) ->
  is_non_negative(binary_to_integer(Bin)).
