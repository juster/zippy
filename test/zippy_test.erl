-module(zippy_test).
-include_lib("eunit/include/eunit.hrl").

object_key_dups_test() ->
    ?assertEqual(
        #{<<"dup">> => 1},
        zippy:json_to_term(<<"{\"dup\": 1, \"dup\": 2, \"dup\": 3}">>)
    ).

str_esc_test() ->
    ?assertEqual(
        <<"\\/", 10, 13, 9, 12, 8, "\"">>,
        zippy:json_to_term(<<"\"\\\\\\/\\n\\r\\t\\f\\b\\\"\"">>)
    ).

str_esc_u_test() ->
    ?assertEqual(
        [<<16#FFFF/utf8>>],
        zippy:json_to_term(<<"[\"\\uFFFF\"]">>)
    ).
