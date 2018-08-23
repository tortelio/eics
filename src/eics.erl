-module(eics).
-include("eics.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([decode/1,
         decode/2,
         encode/1]).

-export_type([todo/0,calendar/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type todo() :: #{
        type    := todo,
        seq     => integer()
       }.

-type calendar() :: #{
        type    := calendar,
        version := binary(),
        prodid  := binary(),
        todos   => [todo()]
       }.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec decode(binary()) -> calendar().
decode(Binary) when is_binary(Binary) ->
    decode(Binary, [{'fold_mode', single}]).

decode(Binary, Opts) when is_binary(Binary) ->
    String = unicode:characters_to_list(Binary, utf8),

    Separator = case lists:nth(1,string:split(String, "\r\n")) of
                    "BEGIN:VCALENDAR" ->
                        "\r\n";
                    _ ->
                        "\n"
                end,

    FoldedLines = string:split(String, Separator, all),
    FoldedLines2 =
    case lists:last(FoldedLines) of
        "" -> lists:droplast(FoldedLines);
        _ -> FoldedLines
    end,

    Lines = unfold(FoldedLines2, Opts),

    % TODO
    Lines2 = lists:filter(fun(L) -> L /= "\r\n" end, lists:map(fun(L) -> L ++ "\r\n" end, Lines)),

    decode_raw_lines(Lines2).

% @doc Encodes a calendar map into an ICS file
-spec encode(map()) -> binary().
encode(CalendarMap) ->
    Mod = #{data => CalendarMap},
    maps:fold(fun(K,V,A) -> create_ics_body(K,V,A) end, <<"">>, Mod).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @doc Creates the ICS body from the map
create_ics_body(Key, Value, Acc) when is_map(Value) ->
    #{type := Type} = Value,
    Component = case lists:member(Type, [calendar, alarm, timezone, event]) of
                    true ->
                        Begin = binary:list_to_bin(string:to_upper(atom_to_list(Type))),
                        <<"V", Begin/binary>>;
                    false ->
                        binary:list_to_bin(string:to_upper(atom_to_list(Type)))
                end,
    BeginComp = <<"BEGIN:", Component/binary, "\r\n">>,
    FullComp = maps:fold(fun(K,V,A) -> create_ics_body(K,V,A) end, BeginComp, Value),
    <<Acc/binary, FullComp/binary, "END:", Component/binary, "\r\n">>;

create_ics_body(Key, Value, Acc) when is_list(Value) ->
    ToIns = case lists:member(Key, [events, timezones]) of
                false ->
                    binary:list_to_bin(lists:flatten(Value));
                true ->
                    Comp = case lists:member(Key, [events, timezones, todos]) of
                               true ->
                                   Begin = binary:list_to_bin(string:to_upper(lists:droplast(atom_to_list(Key)))),
                                   <<"V", Begin/binary>>;
                               false ->
                                   binary:list_to_bin(string:to_upper(lists:droplast(atom_to_list(Key))))
                           end,
                    binary:list_to_bin(lists:map(fun(L) ->
                                                         BeginComp = <<"BEGIN:", Comp/binary, "\r\n">>,
                                                         This = maps:fold(fun(K,V,A) ->create_ics_body(K,V,A) end, BeginComp, L),
                                                         <<This/binary, "END:", Comp/binary, "\r\n">>
                                                 end, Value))
            end,
    <<Acc/binary, ToIns/binary>>;

create_ics_body(_, _, Acc) ->
    Acc.

%% Unfolding lines
%% TODO simplify " " and "\t" matching
unfold([Line | Lines], Opts) ->
    lists:reverse(unfold([Line], Lines, [], Opts)).

unfold(PrevLines, [], UnfoldedLines, _) ->
    append_prev_lines(UnfoldedLines, PrevLines);

unfold(PrevLines, [Line | Lines], UnfoldedLines, Opts) ->
    FoldFun = case proplists:get_value('fold_mode', Opts) of
                  single ->
                      1;
                  multiple ->
                      length(PrevLines)
              end,
    case string:prefix(Line, lists:duplicate(FoldFun, $ )) of
        SubLine when is_list(SubLine)->
            unfold([SubLine | PrevLines], Lines, UnfoldedLines, Opts);
        nomatch ->
            case string:prefix(Line, lists:duplicate(length(PrevLines), $\t)) of
                SubLine when is_list(SubLine)->
                    unfold([SubLine | PrevLines], Lines, UnfoldedLines, Opts);
                nomatch ->
                    UnfoldedLines2 = append_prev_lines(UnfoldedLines, PrevLines),
                    unfold([Line], Lines, UnfoldedLines2, Opts)
            end
    end.

%% Concats the previously stored lines
append_prev_lines(UnfoldedLines, PrevLines) ->
    UnfoldedLine = lists:concat(lists:reverse(PrevLines)),
    [UnfoldedLine | UnfoldedLines].

%% Decodes the unfolded lines with the parser line by line
%% Throws fail on wrong input
decode_raw_lines(Lines) ->
    decode_raw_lines(Lines, []).

decode_raw_lines([], [{calendar, Calendar}]) -> Calendar;
decode_raw_lines([RawLine | Rest], State) ->
    {ok, Component, _} = rfc5545:decode(component, RawLine),
    CurrentComponent = component_to_atom(Component),
    ReturnedState = case CurrentComponent of
                        'begin' ->
                            newComponent(RawLine, State);
                        'end' ->
                            endComponent(State);
                        _ ->
                            addComponentPart(RawLine, CurrentComponent, State)
                    end,
    case ReturnedState of
        {ok, State2} ->
            decode_raw_lines(Rest, State2);
        fail ->
            throw(fail)
    end.

%% If Line starts with 'BEGIN', this function creates the element for it in the State
newComponent(Line, State) ->
    Type = getCompType(Line),
    case Type of
        calendar ->
            {ok, [{calendar, #{type => calendar, todos => [], events => [], timezones => []}}]};
        fail ->
            fail;
        _ ->
            {ok, [{Type, #{type => Type}} | State]}
    end.

%% If Line starts with 'END', this function closes the last element in the State
endComponent([{Type, Element} | State] = FullState) ->
    ReturnState = case Type of
                      todo ->
                          [{calendar, #{todos := Todos} = Calendar}] = State,
                          [{calendar, Calendar#{todos => [Element | Todos]}}];
                      event ->
                          [{calendar, #{events := Events} = Calendar}] = State,
                          [{calendar, Calendar#{events => [Element | Events]}}];
                      timezone ->
                          [{calendar, #{timezones := TZs} = Calendar}] = State,
                          [{calendar, Calendar#{timezones => [Element | TZs]}}];
                      calendar ->
                          FullState;
                      _ ->
                          [{ParentType, ParentElement} | Rest] = State,
                          [{ParentType, ParentElement#{Type => Element}} | Rest]
                  end,
    {ok, ReturnState}.

%% Adds the current line to the state after parsing it
addComponentPart(_, _, []) ->
    fail;
addComponentPart(Line, CompType, [{Type, Element} | State]) ->
    ParserParam = case CompType of %gets the param which is given to the parser
                      sequence ->
                          seq;
                      'last-modified' ->
                          'last-mod';
                      _->
                          case string:prefix(Line, "X-") of
                              nomatch ->
                                  CompType;
                              _ ->
                                  'x-prop'
                          end
                  end,
    case rfc5545:decode(ParserParam, Line) of
        {ok, DecodedLine, _} ->
            CleanedList = clean_list(DecodedLine, CompType),
            {ok, [{Type, Element#{CompType => CleanedList}} | State]};
        fail -> fail
    end.

%% Forms an atom from the component the line represents
component_to_atom(Component) ->
    LowerCased = string:lowercase(Component),
    list_to_atom(LowerCased).

%% At 'BEGIN' lines, this function gets the Calendar component that begins
getCompType(Line) ->
    {ok, [_, "", $:, Type, "\r\n"], ""} = rfc5545:decode('contentline', Line),
    case string:prefix(Type, "V") of
        nomatch ->
            component_to_atom(Type);
        CompName ->
            component_to_atom(CompName)
    end.

%% TODO Finish this
clean_list(InputList, CompType) ->
    NoEmptyLists = remove_empty_lists(InputList).

%% Removes all empty lists from the given list
remove_empty_lists(List) ->
    remove_empty_lists(List, []).

remove_empty_lists([], Filtered) ->
    lists:reverse(Filtered);
remove_empty_lists([Head | Rest], Filtered) ->
    FilteredHead = case is_list(Head) of
                true -> remove_empty_lists(Head, []);
                false -> Head
            end,
    case FilteredHead of
        [] -> remove_empty_lists(Rest, Filtered);
        _ -> remove_empty_lists(Rest, [FilteredHead | Filtered])
    end.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

unfold_test() ->
    ?assertEqual(["BEGIN", "END"],
                 unfold(["BEGIN", "END"])),

    ?assertEqual(["BEGIN", "Linesfolded", "END"],
                 unfold(["BEGIN", "Lines", " folded", "END"])),

    ?assertEqual(["BEGIN", "Line1", "Linesfolded", "Line3", "END"],
                 unfold(["BEGIN", "Line1", "Lines", " folded", "Line3", "END"])).

decode_test() ->
    ?assertEqual([["BEGIN", "VCALENDAR"],
                  ["END", "VCALENDAR"]],
                 decode(<<"BEGIN:VCALENDAR\r\nEND:VCALENDAR">>)),

    ?assertEqual([["BEGIN", "VCALENDAR"],
                  ["X-FOLDED-LINE", "Linesfolded"],
                  ["END", "VCALENDAR"]],
                 decode(<<"BEGIN:VCALENDAR\r\nX-FOLDED-LINE:Lines\r\n folded\r\nEND:VCALENDAR">>)),

    ?assertEqual([["BEGIN", "VCALENDAR"],
                  ["X-FOLDED-LINE-1", "Line1"],
                  ["X-FOLDED-LINE-2", "Linesfolded"],
                  ["X-FOLDED-LINE-3", "Line3"],
                  ["END", "VCALENDAR"]],
                 decode(<<"BEGIN:VCALENDAR\r\nX-FOLDED-LINE-1:Line1\r\nX-FOLDED-LINE-2:Lines\r\n",
                          " folded\r\nX-FOLDED-LINE-3:Line3\r\nEND:VCALENDAR">>)).

-endif.
