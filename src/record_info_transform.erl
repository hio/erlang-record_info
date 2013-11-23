% ----------------------------------------------------------------------------
% record_info_transform.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(record_info_transform).
-export([parse_transform/2]).
-export([format_error/1]).

-export_type([erl_form_record_field/0]).

-include("record_info_internal.hrl").

-type lineno() :: erl_scan:line().

-type erl_form_atom() ::
       {
         Type   :: atom,
         Lineno :: lineno(),
         Value  :: atom()
       }.
-type erl_form_record_field_1() ::
        {
          Type      :: record_field,
          Lineno    :: lineno(),
          FieldName :: erl_form_atom()
        }.
-type erl_form_record_field_2() ::
        {
          Type      :: record_field,
          Lineno    :: lineno(),
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
  lineno :: lineno()
}).

-record(export_spec, {
  name   :: atom(),
  file   :: file:name(),
  lineno :: lineno()
}).

-record(error_descriptor, {
  msg :: string()
}).
-type error_descriptor() :: #error_descriptor{}.
-type err_info() :: {file:name(),[{lineno(), module(), error_descriptor()}]}.

-record(parsed_info, {
  file        = "" :: file:name(),
  record_list = [] :: [{atom(),#record_spec{}}],
  export_list = [] :: [#export_spec{}],
  errors      = [] :: [err_info()],
  warnings    = []
}).

% ----------------------------------------------------------------------------
% format_error(ErrDesc).
% called from compiler framework.
%
-spec format_error(error_descriptor()) -> string().
format_error(ErrDesc) ->
  ErrDesc#error_descriptor.msg.

% ----------------------------------------------------------------------------
% parse_transform.
%
-spec parse_transform(
    Form :: [erl_parse:abstract_form()],
    Opts :: [compile:option()]
  ) -> [erl_parse:abstract_form()].
parse_transform(F, O) ->
  ?LOG_DEBUG("F:~n~p~nO:~n~p~n", [F, O]),
  Result  = [],
  ParsedInfo = #parsed_info {
    file        = "",
    record_list = [],
    export_list = []
  },
  F2 = parse_1(F, Result, ParsedInfo, O),
  F2.

parse_1([{attribute, LineNo, record, {Name, Fields}}=Head | Rest], Result, ParsedInfo, O) ->
  ParsedInfo_2 = read_record(Name, Fields, LineNo, ParsedInfo),
  parse_1(Rest, [Head | Result], ParsedInfo_2, O);

parse_1([{attribute, LineNo, export_record_info, Name} | Rest], Result, ParsedInfo, O) when is_atom(Name) ->
  {Result_2, ParsedInfo_2} = parse_export_record_info([Name], LineNo, Result, ParsedInfo),
  parse_1(Rest, Result_2, ParsedInfo_2, O);

parse_1([{attribute, LineNo, export_record_info, NameList} | Rest], Result, ParsedInfo, O) when is_list(NameList) ->
  {Result_2, ParsedInfo_2} = parse_export_record_info(NameList, LineNo, Result, ParsedInfo),
  parse_1(Rest, Result_2, ParsedInfo_2, O);

parse_1([{attribute, LineNo, import_record_info, {Module,RecName}} | Rest], Result, ParsedInfo, O) when is_atom(Module) andalso is_atom(RecName) ->
  {Result_2, ParsedInfo_2} = parse_import_record_info([{Module, RecName}], LineNo, Result, ParsedInfo),
  parse_1(Rest, Result_2, ParsedInfo_2, O);

parse_1([{attribute, LineNo, import_record_info, NameList} | Rest], Result, ParsedInfo, O) when is_list(NameList) ->
  {Result_2, ParsedInfo_2} = parse_import_record_info(NameList, LineNo, Result, ParsedInfo),
  parse_1(Rest, Result_2, ParsedInfo_2, O);

parse_1([{attribute, _LineNo, file, {FileName, _LineNo2}}=Head | Rest], Result, ParsedInfo, O) ->
  ParsedInfo_2 = ParsedInfo#parsed_info {
    file = FileName
  },
  Result_2 = [Head | Result],
  parse_1(Rest, Result_2, ParsedInfo_2, O);

parse_1([Head | Rest], Result, ParsedInfo, O) ->
  Result_2 = [Head | Result],
  parse_1(Rest, Result_2, ParsedInfo, O);

parse_1([], Result, ParsedInfo, _O) ->
  ?LOG_DEBUG("R:~n~p~n~p~n", [ParsedInfo, Result]),

  Result_2 = lists:reverse(Result),
  case ParsedInfo of
    #parsed_info { export_list = [] } ->
      % nop. no records exported.
      make_compiler_result(Result_2, ParsedInfo);
    _ ->
      {Result_3, ParsedInfo_2} = merge(Result_2, ParsedInfo),
      make_compiler_result(Result_3, ParsedInfo_2)
  end.

make_compiler_result(Result, ParsedInfo) ->
  if
    ParsedInfo#parsed_info.errors /= [] ->
      ErrorList = ParsedInfo#parsed_info.errors,
      WarnList  = ParsedInfo#parsed_info.warnings,
      {error, ErrorList, WarnList};
    ParsedInfo#parsed_info.warnings /= [] ->
      WarnList  = ParsedInfo#parsed_info.warnings,
      {warning, Result, WarnList};
    true -> % otherwise.
      Result
  end.

% ----------------------------------------------------------------------------
% read record information.
% record 情報を保存.
read_record(Name, Fields, LineNo, ParsedInfo) ->
  Record = #record_spec {
    name   = {atom, LineNo, Name},
    fields = Fields,
    lineno = LineNo
  },
  RecordList   = ParsedInfo#parsed_info.record_list,
  RecordList_2 = [{Name, Record} | RecordList ],
  ParsdInfo_2  = ParsedInfo#parsed_info {
    record_list = RecordList_2
  },
  ParsdInfo_2.

% ----------------------------------------------------------------------------
% parse_export_record_info.
%
parse_export_record_info(NameList, LineNo, Result, ParsedInfo) ->
  case read_export_record_info(NameList, LineNo, ParsedInfo) of
    {ok, ParsedInfo_2} ->
      {Result, ParsedInfo_2};
    {ng, ParsedInfo_2} ->
      File = ParsedInfo_2#parsed_info.file,
      Msg = "bad export_record_info attribute",
      ParsedInfo_3 = report_error(File, LineNo, Msg, ParsedInfo_2),
      Error    = {error, {LineNo, ?MODULE, Msg}},
      Result_2 = [Error | Result],
      {Result_2, ParsedInfo_3}
  end.

read_export_record_info(NameList, LineNo, ParsedInfo) ->
  ExportList = ParsedInfo#parsed_info.export_list,
  {OkNg, ExportList_2} = lists:foldl(
    fun
      (Name, {OkNg, AccIn}) when is_atom(Name) ->
        ExportSpec = #export_spec {
          name   = Name,
          file   = ParsedInfo#parsed_info.file,
          lineno = LineNo
        },
        {OkNg, [ExportSpec | AccIn]};
      (_, {_OkNg, AccIn}) ->
        {ng, AccIn}
    end,
    {ok, ExportList},
    NameList
  ),
  ParsdInfo_2 = ParsedInfo#parsed_info {
    export_list = ExportList_2
  },
  {OkNg, ParsdInfo_2}.


% ----------------------------------------------------------------------------
% parse_import_record_info.
%
parse_import_record_info(NameList, LineNo, Result, ParsedInfo) ->
  lists:foldl(fun(Elem, {ResultIn, ParsedInfoIn}) ->
    parse_import_record_info_2(Elem, LineNo, ResultIn, ParsedInfoIn)
  end, {Result, ParsedInfo}, NameList).

parse_import_record_info_2({Module, RecordName}, LineNo, Result, ParsedInfo) when is_atom(Module) andalso is_atom(RecordName) ->
  Decl = Module:record_info({decl, RecordName}),
  Fields = Decl#record_decl.fields,
  Attr = {attribute, LineNo, record, {RecordName, Fields}},
  Result_2 = [Attr | Result],
  {Result_2, ParsedInfo};
parse_import_record_info_2(_ImportInfo, LineNo, Result, ParsedInfo) ->
  File = ParsedInfo#parsed_info.file,
  Msg = "bad import_record_info attribute",
  ParsedInfo_2 = report_error(File, LineNo, Msg, ParsedInfo),
  Error    = {error, {LineNo, ?MODULE, Msg}},
  Result_2 = [Error | Result],
  {Result_2, ParsedInfo_2}.


% ----------------------------------------------------------------------------
% merge.
%
% generate information generating code from record information.
% record 情報から情報取得用コードを生成する.
merge(Result, ParsedInfo) ->
  {RecordList, NgList} = lists:foldl(
    fun(Elem, {OkListIn, NgListIn}) ->
      Name = Elem#export_spec.name,
      case proplists:lookup(Name, ParsedInfo#parsed_info.record_list) of
        none ->
          {OkListIn, [Elem | NgListIn]};
        Tuple ->
          {[Tuple | OkListIn], NgListIn}
      end
    end,
    {[], []},
    ParsedInfo#parsed_info.export_list
  ),

  case NgList of
    [] ->
      merge_2(RecordList, Result, ParsedInfo);
    _ ->
      parse_error_invalid_export(NgList, Result, ParsedInfo)
  end.

merge_2(RecordList, Result, ParsedInfo) ->
  InitLineNo = (element(2, lists:last(RecordList)))#record_spec.lineno,

  {InitFun, InitExport} = make_init_fun(RecordList, InitLineNo),
  ?LOG_DEBUG("X:~n~p~n", [InitFun]),
  Spec = make_spec(RecordList, InitLineNo),

  {Preamble, Follows} = split_preamble(Result),
  Result_2 = Preamble ++ [InitExport, Spec, InitFun] ++ Follows,
  ?LOG_DEBUG("XX:~n~p~n", [Result_2]),
  {Result_2, ParsedInfo}.

parse_error_invalid_export([], Result, ParsedInfo) ->
  {Result, ParsedInfo};
parse_error_invalid_export([ExportSpec | Rest], Result, ParsedInfo) ->
  #export_spec {
    name   = Name,
    file   = File,
    lineno = LineNo
  } = ExportSpec,
  Msg = "exported record "++atom_to_list(Name)++" not found",
  ParsedInfo_2 = report_error(File, LineNo, Msg, ParsedInfo),
  Error = {error, {LineNo, ?MODULE, Msg}},
  Result_2 = [Error | Result],
  parse_error_invalid_export(Rest, Result_2, ParsedInfo_2).

report_error(File, LineNo, Msg, ParsedInfo) ->
  ErrDesc      = #error_descriptor { msg = Msg },
  Error        = {File, [{LineNo, ?MODULE, ErrDesc}]},
  ErrorList    = ParsedInfo#parsed_info.errors,
  ParsedInfo_2 = ParsedInfo#parsed_info {
    errors = [Error | ErrorList]
  },
  ParsedInfo_2.

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

    % record_info({decl, RecordName}) の生成.
    DeclPart = (fun() ->
      RawBody = #record_decl {
        fields = Fields
      },
      Body = form_encode(RawBody, LineNo),
      % {decl, RecName}.
      SortKey = {decl, raw_atom(RecordName)},
      Clause  = {tuple, LineNo, [{atom, LineNo, decl}, RecordName]},
      Clause2 = {clause, LineNo, [Clause], [], [Body]},
      {SortKey, Clause2}
    end)(),

    List_2 = [KeysPart, DeclPart | List_1],
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

  % fun() の生成.
  Clauses = [ Orig || {_SortKey, Orig} <- lists:sort(Clauses_2) ],
  FunName = record_info,
  Arity   = 1,
  InitFun = {function, InitLineNo, FunName, Arity, Clauses},
  InitExport = {attribute, InitLineNo, export, [{FunName,Arity}]},

  {InitFun, InitExport}.

form_encode(Form, LineNo) when is_list(Form) ->
  case is_string(Form) of
    true ->
      {string, LineNo, Form};
    false ->
      lists:foldl(fun(Elem, AccIn) ->
        Encoded = form_encode(Elem, LineNo),
        {cons, LineNo, Encoded, AccIn}
      end, {nil, LineNo}, lists:reverse(Form))
  end;

form_encode(Form, LineNo) when is_tuple(Form) ->
  List_1 = tuple_to_list(Form),
  List_2 = lists:map(fun(E) -> form_encode(E, LineNo) end, List_1),
  {tuple, LineNo, List_2};

form_encode(Form, LineNo) when is_atom(Form) ->
  {atom, LineNo, Form};

form_encode(Form, LineNo) when is_integer(Form) ->
  {integer, LineNo, Form}.

is_string(Text) ->
  lists:all(fun(E) -> is_integer(E) andalso E >= 0 andalso E < 128 end, Text).

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

  % ({decl,RecName}) -> any().
  SpecsForDecl = [
     {type,InitLineNo,'fun',
      [{type,InitLineNo,product,
        [{type,InitLineNo,tuple,
          [{atom,InitLineNo,decl},{type,InitLineNo,atom,[]}]}]},
       {type,InitLineNo,any,[]}]}
  ],

  SpecList = [SpecForList] ++ SpecsForKeys ++ SpecsForValue ++ SpecsForDecl,
  InitSpec = {attribute,InitLineNo,spec, {{record_info,1}, SpecList}},

  InitSpec.

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
