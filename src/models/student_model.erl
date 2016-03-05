-module (student_model).
-include ("campus.hrl").

-export ([new/9,
          parse_req/1,
          save/1,
          find/1,
          ensure_index/0]).

-define (DB, <<"students">>).

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

parse_req(PostVals) ->
  student_model:new(
      proplists:get_value(<<"card_uid">>, PostVals),
      proplists:get_value(<<"first">>, PostVals),
      proplists:get_value(<<"last">>, PostVals),
      proplists:get_value(<<"address1">>, PostVals),
      proplists:get_value(<<"address2">>, PostVals),
      proplists:get_value(<<"city">>, PostVals),
      proplists:get_value(<<"postcode">>, PostVals),
      proplists:get_value(<<"id_num">>, PostVals),
      proplists:get_value(<<"student_num">>, PostVals)
  ).

save(Student) ->
  mongo_worker:save(?DB, Student).

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

format_ts(Model) ->
  UpdatedTS = [{C, qdate:to_date(maps:get(C, Model))} || C <- [<<"updated_at">>, <<"created_at">>]],
  FixedId = [{<<"id">>, maps:get(<<"_id">>, Model)}],
  ?DEBUG("UpdatedTS = ~p", [UpdatedTS]),
  ?DEBUG("FixedId = ~p", [FixedId]),
  M = maps:merge(Model, maps:from_list(lists:append(UpdatedTS, FixedId))),
  maps:remove(<<"_id">>, M).