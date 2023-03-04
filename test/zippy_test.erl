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
        <<0,16#FFFF/utf8>>,
        zippy:json_to_term(<<"\"\\u0000\\uFFFF\"">>)
    ).

str_esc_u_solo_hi_surrogate_test() ->
    ?assertError(
        badunicode,
        zippy:json_to_term(<<"\"\\uD800\"">>)
    ).

str_esc_u_solo_lo_surrogate_test() ->
    ?assertError(
        badunicode,
        zippy:json_to_term(<<"\"\\uDC00\"">>)
    ).

str_esc_u_surrogate_pair_test() ->
    ?assertEqual(
        <<240,157,160,128>>,
        zippy:json_to_term(<<"\"\\uD800\\uDC00\"">>)
    ).
