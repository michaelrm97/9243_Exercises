-module(benchmark_ring).
-export([benchmark_avg/4, run_benchmark/3, client/3]).

fill_with_keys(_, 0) -> ok;
fill_with_keys(Server, N) ->
  ringclient:insert(Server, N, N * N),
  fill_with_keys(Server, N - 1).

query_keys(_, 0) ->
  ok;
query_keys(Server, N) ->
  ringclient:lookup(Server, N),
  query_keys(Server, N - 1).

client(N, P, Server) ->
  query_keys(Server, N),
  P ! done.

spawn_clients(_, 0, _, _, _) ->
  ok;
spawn_clients(N, C, P, [], H) ->
  spawn_clients(N, C, P, H, H);
spawn_clients(N, C, P, S, H) ->
  spawn(?MODULE, client, [N, P, hd(S)]),
  spawn_clients(N, C - 1, P, tl(S), H).

wait_for_clients(0) ->
  ok;
wait_for_clients(C) ->
  receive
    done -> 
      wait_for_clients(C - 1)
  end.

run_benchmark(N, C, S) ->
  spawn_clients(N, C, self(), S, S),
  wait_for_clients(C).

benchmark(N, C, M) ->
  S = ringserv:start_ring(M),
  fill_with_keys(hd(S), N),
  {T, _Result} = timer:tc(?MODULE, run_benchmark, [N, C, S]),
  ringserv:stop_ring(S),
  T / 1000000.

benchmark(_, _, 0, _) -> [];
benchmark(N, C, R, M) ->
  [benchmark(N, C, M)|benchmark(N, C, R-1, M)].

benchmark_avg(N, C, R, M) ->
  lists:sum(benchmark(N, C, R, M)) / R.
