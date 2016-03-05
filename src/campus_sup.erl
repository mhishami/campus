%%%-------------------------------------------------------------------
%% @doc campus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('campus_sup').
-behaviour(supervisor).
-include ("campus.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Serial = ?CHILD(serial_worker, worker),

  %% our mongo pool
  %% get configs
  {ok, [{PoolName, SizeArgs, WorkerArgs}]} = application:get_env(campus, pools),
  PoolArgs = [{name, {local, PoolName}}, {worker_module, mc_worker}] ++ SizeArgs,
  PoolSpecs = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),

  Mongo = ?CHILD(mongo_worker, worker, [PoolName]),

  {ok, { {one_for_all, 0, 1}, [Serial, Mongo, PoolSpecs]} }.

%%====================================================================
%% Internal functions
%%====================================================================
