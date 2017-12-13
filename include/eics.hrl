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
          alarm             :: alarm(),
          x = #{}           :: map()
         }.
-type todo() :: #todo{}.

-record alarm, {
          action            :: atom(),
          trigger           :: calendar:datetime(),
          repeat            :: integer(),
          duration          :: calendar:time()
         }.
-type alarm() :: #alarm{}.

-endif.
