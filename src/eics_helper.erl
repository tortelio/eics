-module(eics_helper).

-export([parse_datetime/1,
         binarify/1]).

-define(B2I(Binary), binary_to_integer(Binary)).
-define(I2B(Integer), integer_to_binary(Integer)).

%% TODO timezone handling
parse_datetime(<<Years:4/binary, Months:2/binary, Days:2/binary, "T",
                 Hours:2/binary, Minutes:2/binary, Seconds:2/binary>>) ->
    {{?B2I(Years), ?B2I(Months), ?B2I(Days)},
     {?B2I(Hours), ?B2I(Minutes), ?B2I(Seconds)}};
parse_datetime(<<Years:4/binary, Months:2/binary, Days:2/binary, "T",
                 Hours:2/binary, Minutes:2/binary, Seconds:2/binary, "Z">>) ->
    {{?B2I(Years), ?B2I(Months), ?B2I(Days)},
     {?B2I(Hours), ?B2I(Minutes), ?B2I(Seconds)}}.

binarify({{_, _, _}, {_, _, _}} = DateTime) ->
    BinDate = qdate:to_string(<<"Ymd">>, DateTime),
    BinTime = qdate:to_string(<<"His">>, DateTime),

    <<BinDate/binary, <<"T">>/binary, BinTime/binary, <<"Z">>/binary>>.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

binarify_test() ->
    ?assertEqual(<<"20001112T130059">>, binarify({{2000, 11, 12}, {13, 00, 59}})),

    ok.

-endif.
