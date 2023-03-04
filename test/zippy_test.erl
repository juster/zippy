-module(zippy_test).
-include_lib("eunit/include/eunit.hrl").

object_key_dups_test() ->
    ?assertEqual(
        #{<<"dup">> => 1},
        zippy:json_to_term(<<"{\"dup\": 1, \"dup\": 2, \"dup\": 3}">>)
    ).
