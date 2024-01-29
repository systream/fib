%%%-------------------------------------------------------------------
%%% @author tihi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fib_blacklist_handler).

-behaviour(gen_server).

%% API
-export([start_link/0, add/1, remove/1, is_blacklisted/1, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE, ?MODULE).

-record(state, {}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec add(non_neg_integer()) -> ok | {error, term()}.
add(BlackListItem) when is_integer(BlackListItem) ->
  gen_server:call(?SERVER, {add, BlackListItem}).

-spec remove(non_neg_integer()) -> ok | {error, term()}.
remove(BlackListItem) when is_integer(BlackListItem) ->
  gen_server:call(?SERVER, {remove, BlackListItem}).

-spec clear() -> ok.
clear() ->
  gen_server:call(?SERVER, clear).

-spec is_blacklisted(pos_integer()) -> boolean().
is_blacklisted(Item) ->
  % can a bloom filter speed up things? maybe not worth it. Should be tested
  case ets:lookup(?ETS_TABLE, Item) of
    [] -> false;
    _ -> true
  end.

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

% @TODO distribution between nodes should be solved, use gossip!?
%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: state()}).
init([]) ->
  ?ETS_TABLE =
    ets:new(?ETS_TABLE, [named_table, protected, {read_concurrency, true}]),
  {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: state()) ->
  {reply, Reply :: term(), NewState :: state()} |
  {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
  {stop, Reason :: term(), NewState :: state()}).
handle_call({add, Number}, _From, State) ->
  true = ets:insert(?ETS_TABLE, {Number, true}),
  {reply, ok, State};
handle_call({remove, Number}, _From, State) ->
  true = ets:delete(?ETS_TABLE, Number),
  {reply, ok, State};
handle_call(clear, _From, State) ->
  true = ets:delete_all_objects(?ETS_TABLE),
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: state()) ->
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: state()}).
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: state()}).
handle_info(_Info, State) ->
  {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
