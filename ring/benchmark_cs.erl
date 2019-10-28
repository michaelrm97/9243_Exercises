-module(benchmark_cs).
-export([benchmark_avg/3, run_benchmark/2, query_keys/1, client/2]).

fill_with_keys(0) ->
  ok;
fill_with_keys(N) ->
  dbclient:insert(N, N * N),
  fill_with_keys(N - 1).

query_keys(0) ->
  ok;
query_keys(N) ->
  dbclient:lookup(N),
  query_keys(N - 1).

client(N, P) ->
  query_keys(N),
  P ! done.

spawn_clients(_, 0, _) ->
  ok;
spawn_clients(N, C, P) ->
  spawn(?MODULE, client, [N, P]),
  spawn_clients(N, C - 1, P).

wait_for_clients(0) ->
  ok;
wait_for_clients(C) ->
  receive
    done -> 
      wait_for_clients(C - 1)
  end.

run_benchmark(N, C) ->
  dbserv:start(),
  fill_with_keys(N),
  spawn_clients(N, C, self()),
  wait_for_clients(C),
  dbclient:stop().

benchmark(N, C) ->
  {T, _Result} = timer:tc(?MODULE, run_benchmark, [N, C]),
  T / 1000000.

benchmark(_, _, 0) -> [];
benchmark(N, C, R) ->
  [benchmark(N, C)|benchmark(N, C, R-1)].

benchmark_avg(N, C, R) ->
  lists:sum(benchmark(N, C, R)) / R.
