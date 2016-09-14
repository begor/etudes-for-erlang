-module(calculus).
-export([deriviative/2]).

%% @doc Calculates deriviative of a function F
%% in the point X.
deriviative(F, X) ->
  Delta = 1.0e-10,
  (F(X + Delta) - F(X)) / Delta.
