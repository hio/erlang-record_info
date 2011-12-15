% ----------------------------------------------------------------------------
% test.
% ----------------------------------------------------------------------------
-module(test).
-export([test/0]).
-include("test.hrl").
-include("record_info.hrl").
-import(record_info, [record_to_proplist/2]).
-import(record_info, [proplist_to_record/3]).

%-spec record_info
%  (list) -> [rec2|rec];
%  ({keys,rec})  -> [a|b];
%  ({keys,rec2}) -> [aaa|bbb];
%  ({value,rec,a}) -> 1;
%  ({value,rec,b}) -> 2;
%  ({value,rec2,aaa}) -> undefined;
%  ({value,rec2,bbb}) -> 1234.

-record(rec, {
  a = 1 :: integer(),
  b = 2 :: integer()
}).

-spec test() -> ok.
test() -> 
  plan(9),

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

  R9 = (fun() -> try
    proplist_to_record([], rec_not_defined, ?MODULE)
  catch
    C:R -> {C,R}
  end end)(),
  disp(9, R9, {error,function_clause}),

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
