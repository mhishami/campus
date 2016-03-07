-module (api_handler).

-include ("campus.hrl").

-export ([init/3]).
-export ([allowed_methods/2]).
-export ([content_types_provided/2]).
-export ([content_types_accepted/2]).
-export ([resource_exists/2]).
% -export ([cors_hook/4]).

-export ([handle_json/2]).
-export ([handle/4]).

init(_Transport, _Req, []) ->
  % For the random number generator:
  {X, Y, Z} = os:timestamp(),
  random:seed(X, Y, Z),
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

% cors_hook(Status, Headers, Body, Req) ->
%   Headers2 = lists:append(cowboy:http_headers(), [
%     {<<"access-control-allow-origin">>, <<"*">>},
%     {<<"Access-Control-Allow-Credentials">>, <<"true">>}
%   ]),
%   cowboy_req:reply(Status, Headers2, Body, Req).

handle_json(Req, State) ->
  {ok, Type, Req2} = cowboy_req:parse_header(<<"content-type">>, Req),
  {Method, Req3} = cowboy_req:method(Req2),
  {Path, Req4} = cowboy_req:path(Req3),

  ?INFO("Handling: State=~p, Type=~p, Method=~p, Path=~p", [State, Type, Method, Path]),
  handle(Method, tl(binary:split(Path, <<"/">>, [global])), Req4, State).

% -----------------------------------------------------------------------------
% Process Handlers
%
handle(<<"GET">>, [<<"api">>, <<"students">>, Uid], Req, State) ->
  ?INFO("handling GET req: ~p", [Uid]),
  % Res = #{first => <<"Hisham">>, last => <<"Ismail">>},
  Res = student_model:find(Uid),
  Req2 = add_header(Req),
  {jsx:encode(Res), Req2, State};

handle(<<"POST">>, [<<"api">>, <<"students">>], Req, State) ->
  ?INFO("Handling POST req...", []),
  {ok, [{Json, true}], Req2} = cowboy_req:body_qs(Req),
  % ?INFO("Json = ~p", [Json]),
  case student_model:parse_req(jsx:decode(Json)) of
    {ok, Student} ->
      % % save the data
      ?INFO("Saving student data ~p", [Student]),
      student_model:save(Student),
      Req3 = add_header(Req2),
      {{true, maps:get(<<"_id">>, Student)}, Req3, State};

    {error, _Student} ->
      ?ERROR("Error in saving student data..."),
      Req3 = add_header(Req2),
      {false, Req3, State}

  end;

  % % return the index
  % Req3 = add_header(Req2),
  % {{true, maps:get(<<"_id">>, Student)}, Req3, State};

handle(<<"PUT">>, [<<"api">>, <<"students">>], Req, State) ->
  ?INFO("Handling PUT req...", []),
  {ok, [{Json, true}], Req2} = cowboy_req:body_qs(Req),

  case student_model:parse_req(jsx:decode(Json)) of
    {ok, Student} ->
      % % save the data
      ?INFO("Saving student data ~p", [Student]),
      student_model:update(Student),
      Req3 = add_header(Req2),
      {true, Req3, State};

    {error, _Student} ->
      ?ERROR("Error in saving student data..."),
      Req3 = add_header(Req2),
      {false, Req3, State}

  end;

handle(Method, Path, Req, State) ->
  ?INFO("Unhandled req: ~p, ~p", [Method, Path]),
  Req2 = add_header(Req),
  {true, Req2, State}.

add_header(Req) ->
  cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req).







