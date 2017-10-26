-module(eics_decoder).
-include("eics.hrl").

-export([decode/1]).

-spec decode(binary()) -> calendar().
decode(Binary) when is_binary(Binary) ->
    Lines1 = re:split(Binary, <<"\r?\n">>),

    % Unfoldin lines
    Lines2 = lists:reverse(
               lists:foldl(fun(<<" ", Rest/binary>>, [PreviousLine | Acc]) ->
                                   [<<PreviousLine/binary, Rest/binary>> | Acc];
                              (<<"\t", Rest/binary>>, [PreviousLine | Acc]) ->
                                   [<<PreviousLine/binary, Rest/binary>> | Acc];
                              (<<>>, Acc) -> Acc;
                              (Line, Acc) -> [Line | Acc]
                           end, [], Lines1)),

    Lines3 = lists:map(fun(Line) ->
                               [Key, Value] = binary:split(Line, <<":">>),
                               case binary:split(Key, <<"X-">>) of
                                   [<<>>, XProp] -> [x, XProp, Value];
                                   _ -> [Key, Value]
                               end
                       end, Lines2),

    decode_calendar(Lines3).

%% CALENDAR
decode_calendar([[?BEGIN, ?CALENDAR] | Rest1]) ->
    {ok, Calendar1, Rest2} = decode_calendar_properties(Rest1, #calendar{}),

    case decode_elements(Rest2, Calendar1) of
        {Calendar2, [[?END, ?CALENDAR]]} -> Calendar2;
        _ -> throw(missing_end_calendar)
    end;
decode_calendar(_) ->
    throw(missing_begin_calendar).

decode_calendar_properties([[?VERSION, Version] | Rest], #calendar{version = undefined} = Calendar) ->
    decode_calendar_properties(Rest, Calendar#calendar{version = Version});

decode_calendar_properties([[?PRODID, ProductionId] | Rest], #calendar{production_id = undefined} = Calendar) ->
    decode_calendar_properties(Rest, Calendar#calendar{production_id = ProductionId});

decode_calendar_properties([[?CALSCALE, CalendarScale] | Rest], #calendar{calendar_scale = undefined} = Calendar) ->
    decode_calendar_properties(Rest, Calendar#calendar{calendar_scale = CalendarScale});

decode_calendar_properties([[?METHOD, Method] | Rest], #calendar{method = undefined} = Calendar) ->
    decode_calendar_properties(Rest, Calendar#calendar{method = Method});

decode_calendar_properties([[x, XProp, XValue] | Rest], #calendar{x = X} = Calendar) ->
    decode_calendar_properties(Rest, Calendar#calendar{x = X#{XProp => XValue}});

decode_calendar_properties([[?BEGIN, _] | _] = Rest, Calendar) ->
    {ok, Calendar, Rest};

decode_calendar_properties([[?END, ?CALENDAR] | _] = Rest, Calendar) ->
    {ok, Calendar, Rest}.

%% ELEMENTS
decode_elements([[?BEGIN, ?EVENT] | _] = Rest1, #calendar{events = Events} = Calendar) ->
    {Event, Rest2} = decode_event(Rest1),
    decode_elements(Rest2, Calendar#calendar{events = [Event | Events]});
decode_elements([[?BEGIN, ?TODO] | _] = Rest1, #calendar{todos = Todos} = Calendar) ->
    {Todo, Rest2} = decode_todo(Rest1),
    decode_elements(Rest2, Calendar#calendar{todos = [Todo | Todos]});
decode_elements([[?END, ?CALENDAR]] = Rest, Calendar) ->
    {Calendar, Rest}.

%% EVENT
decode_event([[?BEGIN, ?EVENT] | Rest]) ->
    decode_event(Rest, #event{}).

decode_event([[?END, ?EVENT] | Rest], Event) -> {Event, Rest};
decode_event([_ | Rest], Event) -> decode_event(Rest, Event).

%% TODO
decode_todo([[?BEGIN, ?TODO] | Rest]) ->
    decode_todo(Rest, #todo{}).

decode_todo([[?END, ?TODO] | Rest], Todo) -> {Todo, Rest};
decode_todo([[?CREATED_AT, Datetime] | Rest], Todo) ->
    decode_todo(Rest, Todo#todo{created_at = eics_helper:parse_datetime(Datetime)});
decode_todo([[?SEQUENCE, Sequence] | Rest], Todo) ->
    decode_todo(Rest, Todo#todo{sequence = binary_to_integer(Sequence)});
decode_todo([[?DUE, Datetime] | Rest], Todo) ->
    decode_todo(Rest, Todo#todo{due = eics_helper:parse_datetime(Datetime)});
decode_todo([[?ID, ID] | Rest], Todo) ->
    decode_todo(Rest, Todo#todo{id = ID});
decode_todo([[?STATUS, Status] | Rest], #todo{status = undefined} = Todo) ->
    decode_todo(Rest, Todo#todo{status = decode_status(Status)});
decode_todo([[?STATUS, _Status] | _Rest], _Todo) ->
    throw(status_already_exists);
decode_todo([[?SUMMARY, Summary] | Rest], Todo) ->
    decode_todo(Rest, Todo#todo{summary = Summary});
% TODO refactor this
decode_todo([[?BEGIN, ?ALARM] | _] = Rest1, Todo) ->
    {Alarm, Rest2} = decode_alarm(Rest1),
    decode_todo(Rest2, Todo#todo{alarm = Alarm});
decode_todo([_ | Rest], Todo) -> decode_todo(Rest, Todo).

%% ALARM
decode_alarm([[?BEGIN, ?ALARM] | Rest]) ->
    decode_alarm(Rest, #alarm{}).

decode_alarm([[?END, ?ALARM] | Rest], Alarm) -> {Alarm, Rest};
decode_alarm([_ | Rest], Alarm) -> decode_alarm(Rest, Alarm).

% STATUS
decode_status(<<"NEEDS-ACTION">>) ->
    needs_action;
decode_status(<<"COMPLETED">>) ->
    completed;
decode_status(<<"IN-PROCESS">>) ->
    in_process;
decode_status(<<"CANCELLED">>) ->
    cancelled;
decode_status(Status) ->
    throw({unknown_todo_status, Status}).
