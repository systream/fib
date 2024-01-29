%%%-------------------------------------------------------------------
%%% @author tihi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fib_cache).

-behaviour(gen_server).

%% API
-export([start_link/0, fetch/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE, ?MODULE).

-record(state, {
    in_process = #{} :: #{Key :: term() => [From :: {pid(), any()}]}
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec fetch(term(), fun((term()) -> term())) -> term().
fetch(Key, FetchFun) ->
  case ets:lookup(?ETS_TABLE, Key) of
      [{_, Result}] ->
        Result;
      [] ->
        gen_server:call(?SERVER, {fetch, Key, FetchFun})
  end.

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: state()}).
init([]) ->
  ?ETS_TABLE = ets:new(?ETS_TABLE, [protected, {read_concurrency, true}, named_table]),
  {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: state()) ->
  {noreply, NewState :: state()}).
handle_call({fetch, Key, FetchFun}, From, #state{in_process = InProcess} = State) ->
  NewSubscribers = case maps:get(Key, InProcess, not_found) of
                      not_found ->
                        % spawn a process to gather the data
                        Parent = self(),
                        spawn_link(fun() -> Parent ! {result, Key, FetchFun(Key)} end),
                        [From];
                      Subscribers ->
                        [From | Subscribers]
                    end,
  NewInProcess = InProcess#{Key => NewSubscribers},
  {noreply, State#state{in_process = NewInProcess}}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: state()) ->
  {noreply, NewState :: state()}).
handle_cast(_Request, #state{} = State) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
  {noreply, NewState :: state()}).
handle_info({result, Key, Result}, #state{in_process = InProcess} = State) ->
  ets:insert(?ETS_TABLE, {Key, Result}),
  [gen_server:reply(From, Result) || From <- maps:get(Key, InProcess, [])],
  {noreply, State#state{in_process = maps:remove(Key, InProcess)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
