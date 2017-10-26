-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../src/eics.hrl").

-define(DATA_DIR(Config), ?config(data_dir, Config)).
-define(READ_FILE(FilePath),
        begin
            (fun(__FP) ->
                     {ok, __FC} = file:read_file(__FP),
                     __FC
             end(FilePath))
        end).
-define(READ_FILE(Filename, Config), ?READ_FILE(filename:join([?DATA_DIR(Config), Filename]))).
