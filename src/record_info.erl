% ----------------------------------------------------------------------------
% record_info.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(record_info).
-export([record_to_proplist/2]).
-export([proplist_to_record/3]).
-export([parse_transform/2]).

-type proplist() :: [{atom(), any()}].
-type a_record() :: tuple().

parse_transform(Form, Opts) ->
  record_info_transform:parse_transform(Form, Opts).

-spec record_to_proplist(
    Record :: a_record(),
    Module :: module()
  ) -> proplist().
record_to_proplist(Record, Module) ->
  [RecordName | Values] = tuple_to_list(Record),
  Keys = Module:record_info({keys, RecordName}),
  lists:zip(Keys, Values).

-spec proplist_to_record(
    List       :: proplist(),
    Module     :: module(),
    RecordName :: atom()
  ) -> a_record().
proplist_to_record(List, Module, RecordName) ->
  Keys = Module:record_info({keys, RecordName}),
  Values = [
    case proplists:lookup(Key, List) of
      {_, Value} ->
        Value;
      none ->
        Module:record_info({value, RecordName, Key});
      Any ->
        erlang:error({badarg, Any}, [List, Module, RecordName])
    end
    || Key <- Keys
  ],
  list_to_tuple([RecordName | Values]).

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
