-module(eics).
-include("eics.hrl").

-export([decode/1]).

-spec decode(binary()) -> any().
decode(Binary) when is_binary(Binary) ->
  not_implemented.
