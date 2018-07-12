-ifndef(EICS_HRL).
-define(EICS_HRL, true).

-type todo() :: #{
        type    => todo,
        seq     => integer()
       }.

-type calendar() :: #{
        type    => calendar,
        version => binary(),
        prodid  => binary(),
        todos   => [todo()]
       }.

-endif.
