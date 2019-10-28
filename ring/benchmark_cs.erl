-module(benchmark_cs).
-export([benchmark/2, query_keys/1, client/1]).

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

client(N) ->
  query_keys(N),
  benchmarker ! done.

spawn_clients(_, 0) ->
  ok;
spawn_clients(N, C) ->
  spawn(?MODULE, client, [N]),
  spawn_clients(N, C - 1).

wait_for_clients(0) ->
  ok;
wait_for_clients(C) ->
  receive
    {done} -> wait_for_clients(C - 1)
  end.

benchmark(N, C) ->
  register(benchmarker, self()),
  dbserv:start(),
  fill_with_keys(N),
  spawn_clients(N, C),
  wait_for_clients(C),
  dbclient:stop().
