-module (powers).
-export ([raise/2]).

%% @doc Raises X to the Nth power.
raise(_, 0) -> 1;
raise(X, 1) -> X;
raise(X, N) when N > 0 -> raise(X, N, 1);
raise(X, N) when N < 0 ->  1.0 / raise(X, -N).

%% @doc Raises x to the Nth power using tail recursion.
raise(_, 0, Acc) -> Acc;
raise(X, N, Acc) -> raise(X, N - 1, X * Acc). 