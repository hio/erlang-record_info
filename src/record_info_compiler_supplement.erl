% ----------------------------------------------------------------------------
% record_info_compiler_supplement.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(record_info_compiler_supplement).
-export([record_fields/2]).

-include("record_info_internal.hrl").

-spec record_fields(
    Mod :: module(),
    RecordName :: atom()
  ) -> {value, [record_info_transform:erl_form_record_field()]}
     | none.
record_fields(Mod, RecordName) ->
  MaybeDecl = get_decl(Mod, RecordName),
  case MaybeDecl of
    {value, #record_decl{ fields = Fields }} ->
      {value, Fields};
    none ->
      none
  end.

get_decl(Mod, RecordName) when is_atom(Mod) andalso is_atom(RecordName) ->
  try
    {value, Mod:record_info({decl, RecordName})}
  catch
    error:undef ->
      % specified module not found, or that module does not have record_info
      % meta data.
      none;
    error:function_clause ->
      % that module compiled with record_info, but specified record is not
      % exported.
      none
  end.
    
% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
