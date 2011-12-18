% ----------------------------------------------------------------------------
% examples/example_2.erl
% ----------------------------------------------------------------------------
-module(example_2).
-export([main/0]).
-include_lib("record_info/include/record_info.hrl").
-import_record_info([{example, file_info}]).

-spec main() -> ok.
main() ->
  FileInfo = #file_info{},
  io:format("FileInfo = ~p~n", [FileInfo]),

  init:stop(),
  ok.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
