-module(eics).
-include("eics.hrl").

-export([decode/1,
         encode/1]).

-export_type([calendar/0]).

-spec decode(binary()) -> not_implemented.
decode(Binary) when is_binary(Binary) ->
    eics_decoder:decode(Binary).

-spec encode(calendar()) -> not_implemented.
encode(Calendar) when is_record(Calendar, calendar) ->
    eics_encoder:encode(Calendar).
