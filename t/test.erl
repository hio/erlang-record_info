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
  R1 = #rec{a = 3, b = 4},
  io:format("R1 = ~p~n", [R1]),
  %==> R1 = {rec,3,4}

  R2 = record_to_proplist(R1, ?MODULE),
  io:format("R2 = ~p~n", [R2]),
  %==> R2 = [{a,3},{b,4}]

  R3 = proplist_to_record(R2, rec, ?MODULE),
  io:format("R3 = ~p~n", [R3]),
  %==> R3 = {rec,3,4}

  R4 = proplist_to_record([], rec, ?MODULE),
  io:format("R4 = ~p~n", [R4]),
  %==> R4 = {rec,1,2}

  R5 = fun()-> try
    % non key-value element.
    BadElem = list_to_tuple([a,b,c]),
    proplist_to_record([BadElem], rec, ?MODULE)
  catch
    C:R -> {C,R}
  end end(),
  io:format("R5 = ~p~n", [R5]),
  %==> R5 = {error,{badarg,{a,b,c}}}


  R6 = #rec2{aaa = 4},
  io:format("R6 = ~p~n", [R6]),
  %==> R6 = {rec2,4,undefined}

  R7 = record_to_proplist(R6, ?MODULE),
  io:format("R7 = ~p~n", [R7]),
  %==> R7 = [{aaa,4},{bbb,undefined}]

  R8 = proplist_to_record([], rec2, ?MODULE),
  io:format("R8 = ~p~n", [R8]),
  %==> R8 = {rec2,undefined,undefined}

  R9 = (fun() -> try
    proplist_to_record([], rec_not_defined, ?MODULE)
  catch
    C:R -> {C,R}
  end end)(),
  io:format("R9 = ~p~n", [R9]),
  %==> R9 = {error,function_clause}

  ok.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
