-module (user_model).
-include ("campus.hrl").

-import (student_model, [validate/1]).
-import (student_model, [format_ts/1]).

-export ([new/2]).
-export ([parse_req/1]).
-export ([save/1]).
-export ([update/1]).
-export ([find/1]).

-define (DB, <<"user">>).

new(Username, Password) ->
  Time = os:timestamp(),
  #{
    <<"_id">> => uuid:gen(),
    <<"username">> => Username,
    <<"password">> => Password,
    <<"created_at">> => Time,
    <<"updated_at">> => Time,
    <<"last_login">> => Time
  }.

parse_req(PostVals) ->
  User = user_model:new(
      proplists:get_value(<<"username">>, PostVals, <<"">>),
      proplists:get_value(<<"password">>, PostVals, <<"">>)
  ),
  case validate(User) of
    ok -> {ok, User};
    error -> {error, User}
  end.

save(User) ->
  mongo_worker:save(User).

update(User) ->
  mongo_worker:update(User).

find(Username) ->
  case mongo_worker:find(?DB, #{<<"username">> => Username}) of
    {ok, User} ->
      format_ts(User);
    {error, _} ->
      []
  end.
