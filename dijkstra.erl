-module(dijkstra).
-export([gcd/2]).


%% @doc Calculate GCD of two ints M and N
%% implementig Dijkstra's algorithm.
gcd(M, M) -> M;
gcd(M, N) when M > N -> gcd(M - N, N);
gcd(M, N) -> gcd(M, N - M).
