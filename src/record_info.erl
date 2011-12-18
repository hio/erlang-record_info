% ----------------------------------------------------------------------------
% record_info.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(record_info).
-export([version/0]).
-export([version_string/0]).
-export([record_list/1]).
-export([field_list/2]).
-export([default_value/3]).
-export([record_to_proplist/2]).
-export([proplist_to_record/3]).

% private.
-export([parse_transform/2]).

-type proplist() :: [{atom(), any()}].
-type a_record() :: tuple().
-type record_name() :: atom().
-type field_name()  :: atom().


-spec version() -> {non_neg_integer(),non_neg_integer(),non_neg_integer(),non_neg_integer()}.
version() ->
  {0,2,0,0}.

-spec version_string() -> string().
version_string() ->
  {Major,Minor,Patch,Build} = version(),
  lists:flatten(io_lib:format("~p.~p.~p.~p", [Major,Minor,Patch,Build])).

-spec record_list(
    Module :: module()
  ) -> [record_name()].
record_list(Module) ->
  Module:record_info(list).

-spec field_list(
    RecordName :: record_name(),
    Module     :: module()
  ) -> [field_name()].
field_list(RecordName, Module) ->
  Module:record_info({keys, RecordName}).

-spec default_value(
    Key        :: field_name(),
    RecordName :: record_name(),
    Module     :: module()
  ) -> any().
default_value(Key, RecordName, Module) ->
  Module:record_info({value, RecordName, Key}).


-spec parse_transform(
    Forms :: [erl_parse:abstract_form()],
    Opts  :: [compile:option()]
  ) -> [erl_parse:abstract_form()].
parse_transform(Form, Opts) ->
  record_info_transform:parse_transform(Form, Opts).

-spec record_to_proplist(
    Record :: a_record(),
    Module :: module()
  ) -> proplist().
record_to_proplist(Record, Module) ->
  [RecordName | Values] = tuple_to_list(Record),
  Keys = field_list(RecordName, Module),
  lists:zip(Keys, Values).

-spec proplist_to_record(
    List       :: proplist(),
    RecordName :: record_name(),
    Module     :: module()
  ) -> a_record().
proplist_to_record(List, RecordName, Module) ->
  Keys = field_list(RecordName, Module),
  Values = [
    case proplists:lookup(Key, List) of
      {_, Value} ->
        Value;
      none ->
        default_value(Key, RecordName, Module);
      Any ->
        erlang:error({badarg, Any}, [List, Module, RecordName])
    end
    || Key <- Keys
  ],
  list_to_tuple([RecordName | Values]).

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
