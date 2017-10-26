-ifndef(EICS_HRL).
-define(EICS_HRL, true).

-record calendar, {
          version           :: string(),
          production_id     :: string(),
          calendar_scale    :: string(),
          method            :: string(),
          x = #{}           :: map(),

          events = []       :: list(),
          todos = []        :: list()
         }.
-type calendar() :: #calendar{}.

-record event, {}.
-type event() :: #event{}.

-record todo, {
          created_at        :: calendar:datetime(),
          sequence          :: integer(),
          id                :: binary(),
          due               :: calendar:datetime(),
          status            :: atom(),
          summary           :: string(),
          alarm             :: alarm()
         }.
-type todo() :: #todo{}.

-record alarm, {
          action            :: atom(),
          trigger           :: calendar:datetime(),
          repeat            :: integer(),
          duration          :: calendar:time()
         }.
-type alarm() :: #alarm{}.

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
