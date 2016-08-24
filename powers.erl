-module (powers).
-export ([raise/2, nth_root/2]).

%%% Public API

%% @doc Raises X to the Nth power.
raise(_, 0) -> 1;
raise(X, 1) -> X;
raise(X, N) when N > 0 -> raise(X, N, 1);
raise(X, N) when N < 0 ->  1.0 / raise(X, -N).

%% @doc Calculates Nth root of a number X
nth_root(X, 1) -> X;
nth_root(X, N) -> nth_root(X, N, X / 2.0).

%%% Helper functions

%% @doc Raises x to the Nth power using tail recursion.
raise(_, 0, Acc) -> Acc;
raise(X, N, Acc) -> raise(X, N - 1, X * Acc). 

nth_root(X, N, A) ->
	io:format("Current guess is ~p~n", [A]),
	Limit = 1.0e-8,
	F = raise(A, N) - X,
	Fprime = N * raise(A, N - 1),
	Next = A - F / Fprime,
	Change = abs(Next - A),
	if Change < Limit -> Next;
	   true -> nth_root(X, N, Next)
	end.
