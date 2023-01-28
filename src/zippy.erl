-module(zippy).
-export([json_to_term/1]).
-nifs([json_to_term/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./zippy", 0).

json_to_term(_) ->
    exit(nif_library_not_loaded).
