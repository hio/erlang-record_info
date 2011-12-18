% ----------------------------------------------------------------------------
% test.
% ----------------------------------------------------------------------------
-module(test).
-export([main/0]).
-export([test/0]).
-include("test.hrl").
-include("record_info.hrl").
-import(record_info, [record_to_proplist/2]).
-import(record_info, [proplist_to_record/3]).
-export_record_info([rec]).
-export_record_info([rec2]).

-record(rec, {
  a = 1 :: integer(),
  b = 2 :: integer()
}).

-record(rec_private, {
  a = 1 :: integer()
}).

-spec main() -> ok.
main() ->
  test(),
  init:stop(),
  ok.

-spec test() -> ok.
test() -> 
  plan(12),

  R1 = #rec{a = 3, b = 4},
  disp(1, R1, {rec, 3, 4}),

  R2 = record_to_proplist(R1, ?MODULE),
  disp(2, R2, [{a,3}, {b,4}]),

  R3 = proplist_to_record(R2, rec, ?MODULE),
  disp(3, R3, {rec, 3, 4}),

  R4 = proplist_to_record([], rec, ?MODULE),
  disp(4, R4, {rec,1,2}),

  R5 = fun()-> try
    % non key-value element.
    BadElem = list_to_tuple([a,b,c]),
    proplist_to_record([BadElem], rec, ?MODULE)
  catch
    C:R -> {C,R}
  end end(),
  disp(5, R5, {error,{badarg,{a,b,c}}}),


  R6 = #rec2{aaa = 4},
  disp(6, R6, {rec2,4,1234}),

  R7 = record_to_proplist(R6, ?MODULE),
  disp(7, R7, [{aaa,4},{bbb,1234}]),

  R8 = proplist_to_record([], rec2, ?MODULE),
  disp(8, R8, {rec2,undefined,1234}),

  R9 = lists:sort(record_info:record_list(?MODULE)),
  disp(9, R9, [rec, rec2]),

  R10 = #rec_private { a = 10 },
  disp(10, R10, #rec_private{ a = 10 }),

  R11 = (fun() -> try
    record_to_proplist(R10, ?MODULE)
  catch
    C:R -> {C,R}
  end end)(),
  disp(11, R11, {error,function_clause}),

  R12 = (fun() -> try
    proplist_to_record([], rec_private, ?MODULE)
  catch
    C:R -> {C,R}
  end end)(),
  disp(12, R12, {error,function_clause}),

  ok.

plan(N) ->
  io:format("1..~p~n", [N]),
  ok.

disp(N, Got, Expected) ->
  Eq = Got == Expected,
  Result = case Eq of
    true  -> "ok";
    false -> "not ok"
  end,
  ExpectedStr = io_lib:format("~p", [Expected]),
  io:format("~s ~p # expects ~s~n", [Result, N, ExpectedStr]),
  case Eq of
    true ->
      ok;
    false ->
      GotStr = io_lib:format("~p", [Got]),
      io:format("# got ~s~n", [GotStr])
  end,
  Eq.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
