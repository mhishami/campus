-module (api_handler).

-include ("campus.hrl").

-export ([init/3]).
-export ([allowed_methods/2]).
-export ([content_types_provided/2]).
-export ([content_types_accepted/2]).
-export ([resource_exists/2]).
% -export ([options/2]).

-export ([to_json/2]).
-export ([from_json/2]).
-export ([process/4]).

init(_Transport, _Req, []) ->
  % For the random number generator:
  {X, Y, Z} = os:timestamp(),
  random:seed(X, Y, Z),
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

% options(Req, State) ->
%   Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, POST, OPTIONS">>, Req),
%   Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req1),
%   Req3 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"application/json">>, Req2),
%   Req4 = cowboy_req:set_resp_header(<<"Accepted">>, <<"application/json">>, Req3),
%   {ok, Req4, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

to_json(Req, State) ->
  {ok, Type, Req2} = cowboy_req:parse_header(<<"content-type">>, Req),
  {Method, Req3} = cowboy_req:method(Req2),
  {Path, Req4} = cowboy_req:path(Req3),

  ?INFO("Processing: State=~p, Type=~p, Method=~p, Path=~p", [State, Type, Method, Path]),
  process(Method, Path, Req4, State).

from_json(Req, State) ->
  {ok, Type, Req2} = cowboy_req:parse_header(<<"content-type">>, Req),
  {Method, Req3} = cowboy_req:method(Req2),
  {Path, Req4} = cowboy_req:path(Req3),

  ?INFO("Processing: State=~p, Type=~p, Method=~p, Path=~p", [State, Type, Method, Path]),
  process(Method, Path, Req4, State).

process(<<"GET">>, <<"/api/students">>, Req, State) ->
  ?INFO("Processing GET req", []),
  Res = #{first => <<"Hisham">>, last => <<"Ismail">>},

  Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),
  {jsx:encode(Res), Req2, State};

process(<<"POST">>, <<"/api/students">>, Req, State) ->
  Data = #{ first => <<"Hisham">>, last => <<"Ismail">> },

  % save the data
  ?INFO("Saving ~p", [Data]),

  % return the index
  Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),
  {{true, 1}, Req2, State};

process(_, _, Req, State) ->
  Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),
  {true, Req2, State}.




