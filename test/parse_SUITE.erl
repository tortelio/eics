-module(parse_SUITE).
-include("eics_test.hrl").

-compile(export_all).

all() -> [parse_calendar,
          parse_calendar_folded_with_htab
         ].

%%==============================================================================
%% SUITE init/end
%%==============================================================================

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%==============================================================================
%% TESTCASE init/end
%%==============================================================================

init_per_testcase(Config) ->
    Config.

end_per_testcase(_Config) ->
    ok.

%%==============================================================================
%% TESTCASE
%%==============================================================================

parse_calendar(Config) ->
    ICS = eics_test_helper:read_suite_data_file("calendar.ics", Config),

    Calendar = eics:decode(ICS),

    ?assertEqual(#{type     => calendar,
                   prodid   => <<"-//ABC Corporation//NONSGML My Product//EN">>,
                   todos    => [#{type      => todo,
                                  sequence  => 2,
                                  summary   => <<"Submit Income Taxes">>,
                                  uid       => <<"uid4@example.com">>,
                                  alarm     => #{type => alarm,
                                                repeat => 4
                                                }
                                 }]
                  }, Calendar),

    ok.

parse_calendar_folded_with_htab(Config) ->
    ICS = eics_test_helper:read_suite_data_file("calendar-folded-with-htab.ics", Config),

    Calendar = eics:decode(ICS),

    ?assertEqual(#{type => calendar,
                   prodid   => <<"-//ABC Corporation//NONSGML My Product//EN">>,
                   todos    => [#{type      => todo,
                                  sequence  => 2,
                                  summary   => <<"Submit Income Taxes">>,
                                  uid       => <<"uid4@example.com">>,
                                  alarm     => #{type => alarm,
                                                repeat => 4
                                                }
                                 }]
                  }, Calendar),

    ok.
