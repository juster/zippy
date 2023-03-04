-module(bench).
-compile(export_all).

start(Filename) ->
    SansExt = filename:rootname(Filename),
    {ok, JsonBin} = file:read_file(["priv/bench/", Filename]),
    %{ok, File} = file:open([SansExt, ".term"], [write]),
    %io:fwrite(File, "~p.~n", [zippy:json_to_term(JsonBin)]),
    %file:close(File),
    %sane(JsonBin),
    meanbench(JsonBin).

sane(JsonBin) ->
    Term1 = jiffy:decode(JsonBin, [return_maps]),
    Term2 = zippy:json_to_term(JsonBin),
    case (Term1 =:= Term2) of
        true -> ok;
        false ->
            io:format("~p~n", [diff(Term1, Term2)]),
            error(insane)
    end.

diff(#{} = Map1, #{} = Map2) ->
    S1 = ordsets:from_list(maps:keys(Map1)),
    S2 = ordsets:from_list(maps:keys(Map2)),
    Only1 = ordsets:subtract(S1, S2),
    Only2 = ordsets:subtract(S2, S1),
    K_Both = ordsets:intersection(S1, S2),
    KV_Both = lists:filtermap(
        fun(K) ->
            #{K := V1} = Map1,
            #{K := V2} = Map2,
            case V1 =:= V2 of
                true -> false;
                false ->
                    case diff(V1, V2) of
                        same -> false;
                        Any -> {true, {K, Any}}
                    end
            end
        end,
        K_Both
    ),
    case {Only1, Only2, KV_Both} of
        {[], [], []} -> #{'$everything' => '$same'};
        _ ->
            maps:from_list(
                [{K, only1} || K <- Only1] ++
                [{K, only2} || K <- Only2] ++
                KV_Both
            )
    end;
diff(L1, L2) when is_list(L1), is_list(L2) ->
    case L1 =:= L2 of
        true -> same;
        false ->
            Delta = length(L2) - length(L1),
            Max = min(length(L1), length(L2)),
            Paired = lists:zip(lists:sublist(L1, Max), lists:sublist(L2, Max)),
            [{Delta, difflen}, [diff(X, Y) || {X, Y} <- Paired]]
    end;
diff(Same, Same) ->
    same;
diff(X, Y) ->
    {typeof(X), typeof(Y)}.

typeof(X) when is_boolean(X) -> boolean;
typeof(X) when is_atom(X) -> atom;
typeof(X) when is_binary(X) -> binary;
typeof(X) when is_integer(X) -> integer;
typeof(X) when is_number(X) -> number;
typeof(X) when is_tuple(X) -> tuple;
typeof(X) when is_list(X) -> list;
typeof(X) when is_map(X) -> map.

spawnbench(JsonBin) ->
    Self = self(),
    lists:foreach(
        fun(_) ->
             spawn_link(fun() ->
                 io:format("~p~n", [meanbench1(3, fun() ->
                     _ = jiffy:decode(JsonBin)
                 end)]),
                 Self ! done
             end),
             receive done -> ok end
        end,
        lists:seq(1, 10)
    ).

meanbench(JsonBin) ->
    Jiffy = meanbench1(1, fun() -> _ = jiffy:decode(JsonBin), ok end),
    io:format("jiffy: ~p~n", [Jiffy]),
    Zippy = meanbench1(1, fun() -> zippy:json_to_term(JsonBin), ok end),
    io:format("zippy: ~p~n", [Zippy]).

meanbench1(N, Fun) ->
    Self = self(),
    spawn_link(fun () ->
        Times = [element(1, timer:tc(Fun)) || _ <- lists:seq(1, N)],
        Mean = trunc(lists:sum(Times) / length(Times)),
        Self ! {done, {Mean, self_info()}}
    end),
    receive {done, Result} -> Result end.

self_info() ->
    process_info(self(), [memory, total_heap_size]).
