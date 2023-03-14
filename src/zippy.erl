-module(zippy).
-export([json_to_term/1]).
-nifs([json_to_term/1]).
-on_load(load_nif/0).

load_nif() ->
    Path = filename:join(priv_path(), "zippy"),
    erlang:load_nif(Path, 0).

json_to_term(_) ->
    exit(nif_library_not_loaded).

priv_path() ->
    case code:lib_dir(?MODULE) of
        {error, badname} ->
            priv_path1();
        Path ->
            filename:join(Path, "priv")
    end.

priv_path1() ->
    Path = code:which(?MODULE),
    filename:absname_join(filename:dirname(Path), "../priv").
