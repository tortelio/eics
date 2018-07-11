-module(eics).
-include("eics.hrl").

-export([decode/1]).

-spec decode(binary()) -> any().
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

  decode_lines(Lines2).


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

decode_lines(Lines) ->
  lists:reverse(decode_lines(Lines, [])).

decode_lines([], Decoded) -> Decoded;
decode_lines([Line | Rest], Decoded) ->

  D =
  case rfc5545:decode(contentline, Line) of
    {ok, [Key, [], $:, Value | _R], []} -> [Key, Value];
    {ok, Things, []} -> Things;
    {ok, _, Unparsed} -> throw({unparsed, Unparsed});
    fail -> throw(fail)
  end,

  decode_lines(Rest, [D | Decoded]).

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