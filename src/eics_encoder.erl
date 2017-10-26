-module(eics_encoder).
-include("eics.hrl").

-export([encode/1]).

-spec encode(calendar()) -> binary().
encode(Calendar) when is_record(Calendar, calendar) ->
    Tokens = encode_calendar(Calendar),

    ct:pal("TOKENS: ~p~n", [Tokens]),
    UnfoldedLines = lists:map(fun tokens_to_line/1, Tokens),
    ct:pal("UNFOLDED LINES: ~p~n", [UnfoldedLines]),
    FoldedLines = lists:map(fun fold_line/1, UnfoldedLines),
    ct:pal("FOLDED LINES: ~p~n", [FoldedLines]),
    << FoldedLine || FoldedLine <- FoldedLines >>.

tokens_to_line([Key, Value]) -> << Key/binary, <<":">>/binary, Value/binary, <<"\n">>/binary>>.

encode_calendar(#calendar{version = Version,
                          production_id = ProdId,
                          todos = Todos}) ->
    [[?BEGIN, ?CALENDAR],
     [?VERSION, Version],
     [?PRODID, ProdId]] ++
    encode_todos(Todos) ++
    [[?END, ?CALENDAR]].

encode_todos(Todos) ->
    lists:foldl(fun(Todo, Acc) ->
                        Encoded = encode_todo(Todo),
                        Acc ++ Encoded
                end, [], Todos).

encode_todo(#todo{
               created_at = CreatedAt,
               sequence = Sequence,
               id = Id,
               summary = Summary,
               due = Due,
               status = Status
              }) ->
    [[?BEGIN, ?TODO],
     [?CREATED_AT, eics_helper:binarify(CreatedAt)],
     [?SEQUENCE, integer_to_binary(Sequence)],
     [?ID, Id],
     [?DUE, eics_helper:binarify(Due)],
     [?STATUS, encode_status(Status)],
     [?SUMMARY, Summary],
     [?END, ?TODO]].

% STATUS
encode_status(needs_action) ->
    <<"NEEDS-ACTION">>;
encode_status(completed) ->
    <<"COMPLETED">>;
encode_status(in_process) ->
    <<"IN-PROCESS">>;
encode_status(cancelled) ->
    <<"CANCELLED">>;
encode_status(Status) ->
    throw({unknown_todo_status, Status}).

fold_line(Binary) -> fold_line(Binary, <<" ">>).

fold_line(<<Chunk:75/binary, Rest/binary>>, Sep) ->
    Rest2 = fold_line(Rest),
    << Chunk/binary, <<"\n">>/binary, Sep/binary, Rest2/binary >>;
fold_line(Rest, _) -> Rest.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

tokens_to_line_test() ->
    ?assertEqual(<<"A:B\n">>, tokens_to_line([<<"A">>, <<"B">>])),

    ok.

fold_line_test() ->
    ?assertEqual(<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n AAAAA">>,
                 fold_line(<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>)),
    ?assertEqual(<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n\tAAAAA">>,
                 fold_line(<<"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA">>, <<"\t">>)),

    ok.

-endif.
