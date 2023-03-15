-module(bench).
-compile(export_all).
-define(WARMUP_ROUNDS, 4).

start(Filename) ->
    SansExt = filename:rootname(Filename),
    {ok, JsonBin} = file:read_file(["priv/bench/", Filename]),
    %{ok, File} = file:open([SansExt, ".term"], [write]),
    %io:fwrite(File, "~p.~n", [zippy:json_to_term(JsonBin)]),
    %file:close(File),
    %sane(JsonBin),
    meanbench(SansExt, JsonBin).

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

meanbench(Name, JsonBin) ->
    Jiffy = bench(timer:seconds(10), 1, 16, fun() -> jiffy:decode(JsonBin), ok end),
    io:format("~s: jiffy: ~p~n", [Name, Jiffy]),
    Zippy = bench(timer:seconds(10), 1, 16, fun() -> zippy:json_to_term(JsonBin), ok end),
    io:format("~s: zippy: ~p~n", [Name, Zippy]).

bench(WaitMs, N, M, Fun) ->
    Self = self(),
    CollectPid = proc_lib:spawn_link(fun() -> collector(N) end),
    register(collector, CollectPid),
    % warmup
    [_ = Fun() || _ <- lists:seq(1, ?WARMUP_ROUNDS)],
    Pid = spawn_link(fun() -> bench_sup(N, M, Fun) end),
    timer:sleep(WaitMs),
    unlink(Pid),
    exit(Pid, kill),
    CollectPid ! {collect, Self},
    receive {benches, Runs} -> Runs end.

bench_sup(N, M, Fun) ->
    process_flag(trap_exit, true),
    [spawn_link(fun() -> runner(N, Fun) end) || _ <- lists:seq(1, M)],
    bench_sup1(N, Fun).

bench_sup1(N, Fun) ->
    receive
        {'EXIT', _Pid, normal} ->
            proc_lib:spawn_link(fun() -> runner(N, Fun) end),
            bench_sup1(N, Fun);
        {'EXIT', _Pid, Reason} ->
            io:format("DBG: Reason: ~p~n", [Reason]),
            exit(Reason)
    end.

runner(N, Fun) ->
    Times = [element(1, timer:tc(Fun)) || _ <- lists:seq(1, N)],
    Mean = trunc(lists:sum(Times) / length(Times)),
    collector ! {bench, [Mean | run_stats()]},
    ok.

run_stats() ->
    [element(2, process_info(self(), K)) || K <- [memory, total_heap_size]].

collector(N) ->
    collector(N, []).

collector(N, L1) ->
    receive
        {bench, L2} ->
            collector(N, [L2 | L1]);
        {collect, Pid} ->
            Pid ! {benches, collector1(N, L1)}
    end.

collector1(_, []) -> [];
collector1(N, Runs) ->
    Sums = lists:foldl(
        fun(L, Sum) ->
            mapn(L, Sum, fun(X, Y) -> X + Y end)
        end,
        hd(Runs),
        tl(Runs)
    ),
    %io:format("DBG: Sums=~p~n", [Sums]),
    Counts = lists:duplicate(length(Sums), length(Runs)),
    [X | L] = mapn(Sums, Counts, fun(X, C) -> X / C end),
    [trunc(X / N) | L].

mapn([], [], _Fun) ->
    [];
mapn([X | L1], [Y | L2], Fun) ->
    [Fun(X, Y) | mapn(L1, L2, Fun)].
