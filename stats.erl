-module (stats).
-export ([minimum/1]).

%% @doc Searches for minimum element in List.
minimum([H|Tl]) -> minimum(H, Tl).

minimum(Min, []) -> Min;
minimum(Min, [H|Tl]) when Min < H -> minimum(Min, Tl);
minimum(_, [H|Tl]) -> minimum(H, Tl).