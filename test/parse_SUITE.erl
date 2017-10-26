-module(parse_SUITE).
-include("eics_test.hrl").

-compile(export_all).

all() -> [%decode_calendar_with_events,
          decode_calendar_as_todo_list,
          encode_calendar_as_todo_list
          %encode_calendar
         ].

%%------------------------------------------------------------------------------
%% SUITE init/end
%%------------------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%------------------------------------------------------------------------------
%% TESTCASE init/end
%%------------------------------------------------------------------------------

init_per_testcase(Config) ->
    Config.

end_per_testcase(_Config) ->
    ok.

decode_calendar_with_events(Config) ->
    Calendar = ?READ_FILE("test.ics", Config),

    ?assertEqual(#calendar{
                    version = <<"2.0">>,
                    production_id = <<"-//Test Corporation//Test Product//EN">>,
                    events = [],
                    todos = []
                   },
                 eics:decode(Calendar)),

    ok.

decode_calendar_as_todo_list(Config) ->
    EncodedCalendar = ?READ_FILE("calendar-as-todo-list.ics", Config),
    CalendarObject = fixtures:calendar(simple),

    ?assertEqual(CalendarObject, eics:decode(EncodedCalendar)),

    ok.

encode_calendar(Config) ->
    Calendar = ?READ_FILE("test.ics", Config),

    ?assertEqual(Calendar,
                 eics:encode(#calendar{
                                version = "2.0",
                                production_id = "-//Test Corporation//Test Product//EN",
                                events = [],
                                todos = []
                               })),

    ok.

encode_calendar_as_todo_list(Config) ->
    CalendarObject = fixtures:calendar(simple),
    EncodedCalendar = ?READ_FILE("calendar-as-todo-list.ics", Config),

    ?assertEqual(EncodedCalendar, eics:encode(CalendarObject)),

    ok.
