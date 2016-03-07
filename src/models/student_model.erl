-module (student_model).
-include ("campus.hrl").

-export ([new/0,
          new/9,
          new/12,
          parse_req/1,
          save/1,
          update/1,
          find/1,
          ensure_index/0]).

-define (DB, <<"students">>).

new() ->
  #{<<"_id">> => uuid:gen(),
    <<"balances">> => 0.0,
    <<"created_at">> => os:timestamp(),
    <<"updated_at">> => os:timestamp()}.

new(Uid, FirstName, LastName, Address1, Address2, City, Postcode, IdNum, StudentNum) ->
  #{<<"_id">> => uuid:gen(),
    <<"card_uid">> => Uid,
    <<"first">> => FirstName,
    <<"last">> => LastName,
    <<"address1">> => Address1,
    <<"address2">> => Address2,
    <<"city">> => City,
    <<"postcode">> => Postcode,
    <<"id_num">> => IdNum,
    <<"student_num">> => StudentNum,
    <<"balances">> => 0.0,
    <<"created_at">> => os:timestamp(),
    <<"updated_at">> => os:timestamp()}.

new(Id, Uid, FirstName, LastName, Address1, Address2, City, Postcode, IdNum, StudentNum, Balances, Updated) ->
  #{<<"_id">> => Id,
    <<"card_uid">> => Uid,
    <<"first">> => FirstName,
    <<"last">> => LastName,
    <<"address1">> => Address1,
    <<"address2">> => Address2,
    <<"city">> => City,
    <<"postcode">> => Postcode,
    <<"id_num">> => IdNum,
    <<"student_num">> => StudentNum,
    <<"balances">> => float(Balances),
    <<"created_at">> => os:timestamp(),
    <<"updated_at">> => os:timestamp()}.

parse_req(PostVals) ->
  Student = student_model:new(
      proplists:get_value(<<"_id">>, PostVals, uuid:gen()),
      proplists:get_value(<<"card_uid">>, PostVals, <<"">>),
      proplists:get_value(<<"first">>, PostVals, <<"">>),
      proplists:get_value(<<"last">>, PostVals, <<"">>),
      proplists:get_value(<<"address1">>, PostVals, <<"">>),
      proplists:get_value(<<"address2">>, PostVals, <<"">>),
      proplists:get_value(<<"city">>, PostVals, <<"">>),
      proplists:get_value(<<"postcode">>, PostVals, <<"">>),
      proplists:get_value(<<"id_num">>, PostVals, <<"">>),
      proplists:get_value(<<"student_num">>, PostVals, <<"">>),
      proplists:get_value(<<"balances">>, PostVals, 0.0),
      os:timestamp()
  ),
  case validate(Student) of
    ok -> {ok, Student};
    error -> {error, Student}
  end.

save(Student) ->
  mongo_worker:save(?DB, Student).

update(Student) ->
  mongo_worker:update(?DB, Student).

find(UID) ->
  case mongo_worker:find_one(?DB, {<<"card_uid">>, UID}, #{}) of
    {error, _} -> [];
    {ok, Student} -> format_ts(Student)
  end.

ensure_index() ->
  mongo_worker:ensure_index(?DB, #{<<"key">> => #{<<"student_num">> => 1},
                                   <<"unique">> => true,
                                   <<"dropDups">> => true}),
  mongo_worker:ensure_index(?DB, #{<<"key">> => #{<<"id_num">> => 1},
                                   <<"unique">> => true,
                                   <<"dropDups">> => true}).

validate(Student) ->
  % a few fields are mandatory
  Keys = maps:keys(Student),
  not_empty(Keys, Student).

not_empty([H|T], Student) ->
  case maps:get(H, Student) of
    <<"">> -> error;
    _ -> not_empty(T, Student)
  end;

not_empty([], _Student) -> ok.

format_ts(Model) ->
  UpdatedTS = [{C, qdate:to_date(maps:get(C, Model))} || C <- [<<"updated_at">>, <<"created_at">>]],
  % FixedId = [{<<"id">>, maps:get(<<"_id">>, Model)}],
  ?DEBUG("UpdatedTS = ~p", [UpdatedTS]),
  maps:merge(Model, maps:from_list(UpdatedTS)).
  % ?DEBUG("FixedId = ~p", [FixedId]),
  % M = maps:merge(Model, maps:from_list(UpdatedTS)), lists:append(UpdatedTS, FixedId))),
  % maps:remove(<<"_id">>, M).