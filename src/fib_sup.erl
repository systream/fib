%%%-------------------------------------------------------------------
%% @doc fib top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fib_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      #{id => blacklist_handler,
             start => {fib_blacklist_handler, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [fib_blacklist_handler]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
