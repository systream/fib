%%%-------------------------------------------------------------------
%%% @author tihi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fib_calculator).

%% API
-export([calculate/1, calculate_seq/3]).

% @TODO cache!?
-spec calculate(non_neg_integer()) -> non_neg_integer().
calculate(1) ->
  1;
calculate(0) ->
  0;
calculate(N) ->
  calculate(N, 0, 1).

-spec calculate(non_neg_integer(), non_neg_integer(), pos_integer()) ->
  non_neg_integer().
calculate(0, Result, _) ->
  Result;
calculate(N, Result, Next) ->
  calculate(N - 1, Next, Result + Next).

-spec calculate_seq(pos_integer(), pos_integer(), pos_integer()) -> [pos_integer()].
calculate_seq(Offset, Limit, Target) ->
  lists:reverse(do_calculate_seq(1, Offset, Limit, Target, [])).

-spec do_calculate_seq(Current, Offset, Limit, Target, Acc) -> Result
  when
  Current :: pos_integer(),
  Offset :: non_neg_integer(),
  Limit :: non_neg_integer(),
  Target :: pos_integer(),
  Result :: [pos_integer()],
  Acc :: [pos_integer()].
do_calculate_seq(Current, _Offset, _Limit, Target, Acc) when Target < Current ->
  Acc;
do_calculate_seq(_Current, 0, 0, _Target, Acc) ->
  Acc;
do_calculate_seq(Current, Offset, Limit, Target, Acc) ->
  Result = calculate(Current),
  case fib_blacklist_handler:is_blacklisted(Result) of
    false when Offset > 0 ->
      do_calculate_seq(Current + 1, Offset - 1, Limit, Target, Acc);
    false when Limit > 0 ->
      do_calculate_seq(Current + 1, Offset, Limit - 1, Target, [Result | Acc]);
    true ->
      do_calculate_seq(Current + 1, Offset, Limit, Target, Acc)
  end.
