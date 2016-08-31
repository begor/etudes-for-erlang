-module(cards).
-export([make_deck/0, shuffle/1, rank/1, make_small_deck/0]).

%% @doc Generate a deck of cards as a list 52 tuples.
make_deck() ->
  Suits = ["Clubs", "Diamonds", "Hearts", "Spades"],
  Ranks = [2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K", "A"],
  [{R, S} || R <- Ranks, S <- Suits].

make_small_deck() ->
  Suits = ["Clubs", "Hearts"],
  Ranks = [2, 3, "K", "A"],
  [{R, S} || R <- Ranks, S <- Suits].

%% @doc Shuffles a list of cards.
shuffle(List) -> shuffle(List, []).

%% @doc Tail recursively generates a shuffled list.
%% Algorithm:
%% 0. Given an empty list, return Acc, otherwise:
%% 1. Splits list into two parts randomly (position of split decided randomly) A and B
%% 2. Recursively call itself with List = A + tl(B) and Acc = [hd(B) | Acc],
%%    so each step adds random element (head of a B part of a list) into Acc.
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).

rank("A") -> 14;
rank("K") -> 13;
rank("Q") -> 12;
rank("J") -> 11;
rank(N) -> N.