-module(eics).
-include("eics.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([decode/1]).
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

-spec decode(binary()) -> calendar().
decode(Binary) when is_binary(Binary) ->
    String = unicode:characters_to_list(Binary, utf8),

    FoldedLines = string:split(String, "\r\n", all),
    FoldedLines2 =
    case lists:last(FoldedLines) of
        "" -> lists:droplast(FoldedLines);
        _ -> FoldedLines
    end,

    Lines = unfold(FoldedLines2),

    % TODO
    Lines2 = lists:map(fun(L) -> L ++ "\r\n" end, Lines),

    decode_raw_lines(Lines2).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% Unfolding lines
%% TODO simplify " " and "\t" matching
unfold([Line | Lines]) ->
    lists:reverse(unfold([Line], Lines, [])).

unfold(PrevLines, [], UnfoldedLines) ->
    append_prev_lines(UnfoldedLines, PrevLines);
unfold(PrevLines, [Line | Lines], UnfoldedLines) ->
    case string:prefix(Line, lists:duplicate(length(PrevLines), $ )) of
        SubLine when is_list(SubLine)->
            unfold([SubLine | PrevLines], Lines, UnfoldedLines);
        nomatch ->
            case string:prefix(Line, lists:duplicate(length(PrevLines), $\t)) of
                SubLine when is_list(SubLine)->
                    unfold([SubLine | PrevLines], Lines, UnfoldedLines);
                nomatch ->
                    UnfoldedLines2 = append_prev_lines(UnfoldedLines, PrevLines),
                    unfold([Line], Lines, UnfoldedLines2)
            end
    end.

append_prev_lines(UnfoldedLines, PrevLines) ->
    UnfoldedLine = lists:concat(lists:reverse(PrevLines)),
    [UnfoldedLine | UnfoldedLines].

decode_raw_lines(Lines) ->
    decode_raw_lines(Lines, []).

decode_raw_lines([], [{calendar, Calendar}]) -> Calendar;
decode_raw_lines([RawLine | Rest], State) ->
    case rfc5545:decode(contentline, RawLine) of
        {ok, Line, []} ->
            State2 = decode_line(Line, State),
            decode_raw_lines(Rest, State2);
        {ok, _, Unparsed} ->
            throw({unparsed, Unparsed});
        fail ->
            throw(fail)
    end.

decode_line(["BEGIN", "", $:, "VCALENDAR", "\r\n"], []) ->
    [{calendar, #{type => calendar, todos => []}}];

decode_line(["PRODID", "", $:, Prodid, "\r\n"], [{calendar, Calendar}]) ->
    [{calendar, Calendar#{prodid => unicode:characters_to_binary(Prodid)}}];

%% VTODO
decode_line(["BEGIN", "", $:, "VTODO", "\r\n"], State) ->
    [{todo, #{type => todo}} | State];

decode_line(["END", "", $:, "VTODO", "\r\n"], [{todo, Todo}, {calendar, #{todos := Todos} = Calendar}]) ->
    [{calendar, Calendar#{todos => [Todo | Todos]}}];

decode_line(["SEQUENCE", [], $:, Sequence, "\r\n"], [{Type, Element}|State]) ->
    [{Type, Element#{sequence => list_to_integer(Sequence)}}|State];

decode_line(Line, State) ->
    ct:pal("WARNING: unhandled line: ~p~n", [Line]),
    ct:pal("with state: ~p~n", [State]),
    State.

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
