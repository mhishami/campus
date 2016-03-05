-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define (TAG, <<"READ">>).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  erlang:start_timer(1000, self(), ?TAG),
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, _Msg}, Req, State) ->
  {text, Text} = serial_read(),
  erlang:start_timer(1000, self(), ?TAG),
  {reply, {text, Text}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

% =============================================================================
% Private functions
serial_read() ->
  case serial_worker:read() of
    ok -> {text, ?TAG};
    {ok, UID} -> {text, UID}
  end.
