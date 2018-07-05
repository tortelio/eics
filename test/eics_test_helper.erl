-module(eics_test_helper).

-include("eics_test.hrl").

-export([suite_data_dir/1,
         read_suite_data_file/2
        ]).

suite_data_dir(Config) -> ?config(data_dir, Config).

read_suite_data_file(Filename, Config) ->
  Path = filename:join([suite_data_dir(Config), Filename]),
  {ok, Content} = file:read_file(Path),
  Content.
