-ifndef(EICS_PRIVATE_HRL).
-define(EICS_PRIVATE_HRL, true).

-include("../include/eics.hrl").

-define(IS(Subject, RE),
        (fun(__S, __RE) ->
                 re:run(__S, __RE, [{capture, none}]) =:= match
         end)(Subject, RE)).

% ICS tokens
-define(BEGIN,      <<"BEGIN">>).
-define(END,        <<"END">>).

% ICS objects
-define(CALENDAR,   <<"VCALENDAR">>).
-define(EVENT,      <<"VEVENT">>).
-define(TODO,       <<"VTODO">>).
-define(ALARM,      <<"VALARM">>).

% Calendar properties
-define(VERSION,    <<"VERSION">>).
-define(PRODID,     <<"PRODID">>).
-define(CALSCALE,   <<"CALSCALE">>).
-define(METHOD,     <<"METHOD">>).

% Todo properties
-define(CREATED_AT, <<"DTSTAMP">>).
-define(SEQUENCE,   <<"SEQUENCE">>).
-define(ID,         <<"UID">>).
-define(SUMMARY,    <<"SUMMARY">>).
-define(DUE,        <<"DUE">>).
-define(STATUS,     <<"STATUS">>).

-endif. % ifndef(EICS_HRL).
