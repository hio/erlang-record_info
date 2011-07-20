% ----------------------------------------------------------------------------
% record_info_transform.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(record_info_transform).
-export([parse_transform/2]).

-type erl_form_atom() ::
       {
         Type   :: atom,
         Lineno :: pos_integer(),
         Value  :: atom()
       }.
-type erl_form_record_field_1() ::
        {
          Type      :: record_field,
          Lineno    :: pos_integer(),
          FieldName :: erl_form_atom()
        }.
-type erl_form_record_field_2() ::
        {
          Type      :: record_field,
          Lineno    :: pos_integer(),
          FieldName :: erl_form_atom(),
          IniValue  :: erl_parse:abstract_form()
        }.
-type erl_form_record_field() ::
        erl_form_record_field_1() |
        erl_form_record_field_2() .

-define(LOG_DEBUG(F,A), (case os:getenv("TRACE") of
    false -> ok;
    ""    -> ok;
    "0"   -> ok;
    _     -> io:format(F, A)
  end)).

-record(record_spec, {
  name   :: erl_form_atom(),
  fields :: [erl_form_record_field()],
  lineno :: pos_integer()
}).

-spec parse_transform(
    Form :: [erl_parse:abstract_form()],
    Opts :: [compile:option()]
  ) -> [erl_parse:abstract_form()].
parse_transform(F, O) ->
  ?LOG_DEBUG("F:~n~p~nO:~n~p~n", [F, O]),
  Result  = [],
  Records = [],
  F2 = parse_1(F, Result, Records, O),
  F2.

parse_1([{attribute, LineNo, record, {Name, Fields}}=Head | Rest], Result, Records, O) ->
  Records_2 = read_record(Name, Fields, LineNo, Records),
  parse_1(Rest, [Head | Result], Records_2, O);

parse_1([Head | Rest], Result, Records, O) ->
  parse_1(Rest, [Head | Result], Records, O);

parse_1([], Result, Records, _O) ->
  ?LOG_DEBUG("R:~n~p~n", [Records]),

  Result_2 = lists:reverse(Result),
  case Records of
    [] ->
      Result_2;
    _ ->
      merge(Result_2, Records)
  end.

% read record information.
% record 情報を保存.
read_record(Name, Fields, LineNo, Records) ->
  Record = #record_spec {
    name   = {atom, LineNo, Name},
    fields = Fields,
    lineno = LineNo
  },
  [{Name, Record} | Records].

% generate information generating code from record information.
% record 情報から情報取得用コードを生成する.
merge(Result, Records) ->
  InitLineNo = (element(2, lists:last(Records)))#record_spec.lineno,

  {InitFun, InitExport} = make_init_fun(Records, InitLineNo),
  ?LOG_DEBUG("X:~n~p~n", [InitFun]),
  Spec = make_spec(Records, InitLineNo),

  {Preamble, Follows} = split_preamble(Result),
  Result_2 = Preamble ++ [InitExport, Spec, InitFun] ++ Follows,
  ?LOG_DEBUG("XX:~n~p~n", [Result_2]),
  Result_2.


-spec field_name(
    RecordField :: erl_form_record_field()
  ) -> {erl_form_atom(), erl_parse:abstract_form()}.
field_name({record_field, _Line, Name}) ->
  Name;
field_name({record_field, _Line, Name, _Value}) ->
  Name.

-spec field_name_value(
    RecordField :: erl_form_record_field()
  ) -> {erl_form_atom(), erl_parse:abstract_form()}.
field_name_value({record_field, _Line1, Name1}) ->
  {atom, Line2, _Name2} = Name1,
  {Name1, {atom, Line2, undefined}};
field_name_value({record_field, _Line1, Name, Value}) ->
  {Name, Value}.

raw_atom({atom, _Lineno, Atom}) ->
  Atom.

% generate record_init function.
% record_init 関数の作成.
make_init_fun(Records, InitLineNo) ->
  Clauses_0 = [],
  Clauses_1 = lists:foldl(fun({_RecordNameAtom, Spec}, AccIn1) ->
    Fields = Spec#record_spec.fields,
    LineNo = Spec#record_spec.lineno,
    RecordName = Spec#record_spec.name,
    % record_info({value, RecordName, FieldName}) の生成.
    List_1 = lists:foldl(fun(Field, AccIn2) ->
      {FieldName, IniValue} = field_name_value(Field),
      % {value, RecName, FldName}.
      SortKey = {value, raw_atom(RecordName), raw_atom(FieldName)},
      Clause  = {tuple, LineNo, [{atom, LineNo, value}, RecordName, FieldName]},
      Clause2 = {clause, LineNo, [Clause], [], [IniValue]},
      [{SortKey, Clause2} | AccIn2]
    end, AccIn1, Fields),
    % record_info({keys, RecordName}) の生成.
    KeysPart = (fun() ->
      KeysValue = lists:foldl(fun(Field, AccIn2) ->
        FieldName = field_name(Field),
        {cons, LineNo, FieldName, AccIn2}
      end, {nil, LineNo}, lists:reverse(Fields)),
      % {keys, RecName}.
      SortKey = {keys, raw_atom(RecordName)},
      Clause  = {tuple, LineNo, [{atom, LineNo, keys}, RecordName]},
      Clause2 = {clause, LineNo, [Clause], [], [KeysValue]},
      {SortKey, Clause2}
    end)(),
    List_2 = [KeysPart | List_1],
    List_2
  end, Clauses_0, Records),

  % record_info(list) の生成.
  Clauses_2 = (fun() ->
    LineNo = InitLineNo,
    RecordNameList = lists:foldl(fun({_NameAtom, Spec}, AccIn) ->
      {cons, LineNo, Spec#record_spec.name, AccIn}
    end, {nil, LineNo}, Records),
    % list.
    SortKey = list,
    Clause  = {atom, LineNo, list},
    Clause2 = {clause, LineNo, [Clause], [], [RecordNameList]},
    [{SortKey, Clause2} | Clauses_1]
  end)(),
  Clauses = [ Orig || {_SortKey, Orig} <- lists:sort(Clauses_2) ],

  FunName = record_info,
  Arity   = 1,
  InitFun = {function, InitLineNo, FunName, Arity, Clauses},
  InitExport = {attribute, InitLineNo, export, [{FunName,Arity}]},

  {InitFun, InitExport}.

make_spec(Records, InitLineNo) ->
  RecordListType = {type,InitLineNo,list,[{type,11,union,[ {atom,InitLineNo,RecName} || {RecName,_MySpec} <- Records]}]},
  % (list) -> [record_name()].
  SpecForList = {
    type, InitLineNo, 'fun',
    [{type,InitLineNo,product,[{atom,InitLineNo,list}]},RecordListType]
  },

  % ({keys,RecName}) -> [atom()].
  SpecsForKeys = [
     begin
       FldNameList = [ element(3, X) || X<-MySpec#record_spec.fields],
       FldList = [ {atom,InitLineNo,FldName} || {atom,_,FldName} <- FldNameList ],
       {type,InitLineNo,'fun',
        [{type,InitLineNo,product,[{type,InitLineNo,tuple,[{atom,InitLineNo,keys},{atom,InitLineNo,RecName}]}]},
         {type,InitLineNo,list,[{type,InitLineNo,union,FldList}]}]
       }
     end
     ||
     {RecName, MySpec} <- Records
  ],

  % ({value,RecName,Key}) -> any().
  SpecsForValue = [
     {type,InitLineNo,'fun',
      [{type,InitLineNo,product,
        [{type,InitLineNo,tuple,
          [{atom,InitLineNo,value},{type,InitLineNo,atom,[]},{type,InitLineNo,atom,[]}]}]},
       {type,InitLineNo,any,[]}]}
  ],
  SpecList = [SpecForList] ++ SpecsForKeys ++ SpecsForValue,
  InitSpec = {attribute,InitLineNo,spec, {{record_info,1}, SpecList}},

  InitSpec.

%make_spec() ->
%  ok.
% {attribute,11,spec,
%  {{record_info,1},
%   [{type,11,'fun',
%     [{type,11,product,[{atom,11,list}]},{type,11,list,[{type,11,atom,[]}]}]},
%    {type,12,'fun',
%     [{type,12,product,[{type,12,tuple,[{atom,12,keys},{type,12,atom,[]}]}]},
%      {type,12,list,[{type,12,atom,[]}]}]},
%    {type,13,'fun',
%     [{type,13,product,
%       [{type,13,tuple,
%         [{atom,13,value},{type,13,atom,[]},{type,13,atom,[]}]}]},
%      {type,13,any,[]}]}]}},
%
% {attribute,11,spec,
%  {{record_info,1},
%   [{type,11,'fun',
%     [{type,11,product,[{atom,11,list}]},
%      {type,11,list,[{type,11,union,[{atom,11,rec2},{atom,11,rec}]}]}]},
%    {type,12,'fun',
%     [{type,12,product,[{type,12,tuple,[{atom,12,keys},{atom,12,rec}]}]},
%      {type,12,list,[{type,12,union,[{atom,12,a},{atom,12,b}]}]}]},
%    {type,13,'fun',
%     [{type,13,product,[{type,13,tuple,[{atom,13,keys},{atom,13,rec2}]}]},
%      {type,13,list,[{type,13,union,[{atom,13,aaa},{atom,13,bbb}]}]}]},
%    {type,14,'fun',
%     [{type,14,product,
%       [{type,14,tuple,[{atom,14,value},{atom,14,rec},{atom,14,a}]}]},
%      {integer,14,1}]},
%    {type,15,'fun',
%     [{type,15,product,
%       [{type,15,tuple,[{atom,15,value},{atom,15,rec},{atom,15,b}]}]},
%      {integer,15,2}]},
%    {type,16,'fun',
%     [{type,16,product,
%       [{type,16,tuple,[{atom,16,value},{atom,16,rec2},{atom,16,aaa}]}]},
%      {atom,16,undefined}]},
%    {type,17,'fun',
%     [{type,17,product,
%       [{type,17,tuple,[{atom,17,value},{atom,17,rec2},{atom,17,bbb}]}]},
%      {integer,17,1234}]}]}},

% split into preamble part and functions part.
% preamble 部とfunction以降とで分割.
split_preamble(List) ->
  split_preamble_2(List, []).

split_preamble_2([ {attribute,_Line,_Attr,_Value}=Head | Rest], Result) ->
  split_preamble_2(Rest, [Head | Result]);

split_preamble_2(Follows, Result) ->
  {lists:reverse(Result), Follows}.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
