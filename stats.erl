-module (stats).
-export ([minimum/1, maximum/1, range/1]).

%% @doc Searches for a minimum element in a List.
minimum([H|Tl]) -> minimum(H, Tl).

%% @doc Searches for a maximum element in a List.
maximum([H|Tl]) -> maximum(H, Tl).

%% @doc Returns a list of minimum and maximum entries in the list.
range([H|Tl]) -> [minimum(H, Tl), maximum(H, Tl)].  

minimum(Min, []) -> Min;
minimum(Min, [H|Tl]) when Min < H -> minimum(Min, Tl);
minimum(_, [H|Tl]) -> minimum(H, Tl).

maximum(Max, []) -> Max;
maximum(Max, [H|Tl]) when Max > H -> maximum(Max, Tl);
maximum(_, [H|Tl]) -> maximum(H, Tl).