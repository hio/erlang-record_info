% ----------------------------------------------------------------------------
% record_info_internal.hrl.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-ifndef(record_info_internal_hrl).
-define(record_info_internal_hrl, 1).

-record(record_decl, {
  fields = [] :: [record_info_transform:erl_form_record_field()]
}).

-endif.
% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
