%%%-------------------------------------------------------------------
%% @doc campus public API
%% @end
%%%-------------------------------------------------------------------

-module('campus_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  application:ensure_all_started(cowboy),
  application:ensure_all_started(mongodb),
  application:start(sync),
  sync:go(),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, campus, "dist/index.html"}},
      {"/api/[...]", api_handler, []},
      {"/ws", ws_handler, []},
      {"/static/[...]", cowboy_static, {priv_dir, campus, "dist",
        [{mimetypes, cow_mimetypes, all}]}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 3000}], [
    {env, [
      {dispatch, Dispatch},
      {cors_policy, api_policy}
    ]},
    {middlewares, [
      cowboy_router,
      cowboy_cors,
      cowboy_handler
    ]}
    % {onresponse, fun api_handler:cors_hook/4}
  ]),
  'campus_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
