-module(eics).
-include("eics.hrl").

-export([decode/1,
         decode/2,
         encode/1]).

-export([x/1]).

-export_type([calendar/0]).

-spec decode(binary()) -> any().
decode(Binary) when is_binary(Binary) ->
    eics_decoder:decode(Binary).

-spec decode(atom(), binary()) -> any().
decode(Element, Binary) when is_atom(Element) andalso is_binary(Binary) ->
    eics_decoder:decode(Element, Binary).

-spec encode(calendar()) -> not_implemented.
encode(Calendar) when is_record(Calendar, calendar) ->
    eics_encoder:encode(Calendar).

x(#todo{x = X}) -> X.
