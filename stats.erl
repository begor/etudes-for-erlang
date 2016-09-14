-module(stats).
-export([minimum/1, maximum/1, range/1, mean/1, stdv/1, test/0]).

%% @doc Searches for a minimum element in a List.
minimum(List) ->
  try
    minimum(hd(List), tl(List))
  catch
    error:Reason -> {error, Reason}
  end.

%% @doc Searches for a maximum element in a List.
maximum(List) ->
  try
    maximum(hd(List), tl(List))
  catch
    error:Reason -> {error, Reason}
  end.

%% @doc Returns a list of minimum and maximum entries in the list.
range([H | Tl]) -> [minimum(H, Tl), maximum(H, Tl)].

minimum(Min, []) -> Min;
minimum(Min, [H | Tl]) when Min < H -> minimum(Min, Tl);
minimum(_, [H | Tl]) -> minimum(H, Tl).

maximum(Max, []) -> Max;
maximum(Max, [H | Tl]) when Max > H -> maximum(Max, Tl);
maximum(_, [H | Tl]) -> maximum(H, Tl).

%% @doc Calculates the mean for a list of Numbers.
mean(Numbers) ->
  try
    lists:foldl(fun(X, Y) -> X + Y end, 0, Numbers) / length(Numbers)
  catch
    error:Reason -> {error, Reason}
  end.

%% @doc Calculates the standard deviation of Numbers.
stdv(Numbers) ->
  N = length(Numbers),
  try
    {Sum, SumSquares} = lists:foldl(fun(X, {Sum, SumSqr}) -> {Sum + X, SumSqr + X * X} end, {0, 0}, Numbers),
    math:sqrt((N * SumSquares - Sum * Sum) / (N * (N - 1)))
  catch
    error:Reason -> {error, Reason}
  end.


%% @doc Simple testing function
test() ->
  {error, badarg} = minimum([]),
  1 = minimum([1, 2, 3]),
  3 = minimum([4, 3, 8]),

  {error, badarg} = maximum([]),
  3 = maximum([1, 2, 3]),
  8 = maximum([4, 3, 8]),

  {error, badarith} = mean(["212", 423]),
  2.0 = mean([1, 2, 3]),
  5.0 = mean([4, 3, 8]),

  {error, badarith} = stdv([]),
  ok.