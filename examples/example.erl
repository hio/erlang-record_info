% ----------------------------------------------------------------------------
% examples/example.erl
% ----------------------------------------------------------------------------
-module(example).
-export([main/0]).
-include_lib("record_info/include/record_info.hrl").
-include_lib("kernel/include/file.hrl").
-export_record_info([file_info]).

-spec main() -> ok.
main() ->
  {ok, FileInfo} = file:read_file_info("src/record_info.erl"),
  io:format("FileInfo = ~p~n", [FileInfo]),

  AsPropList = record_info:record_to_proplist(FileInfo, ?MODULE),
  io:format("AsPropList = ~p~n", [AsPropList]),

  FileInfoAgain = record_info:proplist_to_record(AsPropList, file_info, ?MODULE),
  io:format("FileInfoAgain = ~p~n", [FileInfoAgain]),
  io:format("RoundTrip = ~p~n", [FileInfoAgain == FileInfo]),

  init:stop(),
  ok.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
