-module(cards).
-export([make_deck/0]).

%% @doc Generate a deck of cards as a list 52 tuples.
make_deck() ->
  Suits = ["Clubs", "Diamonds", "Hearts", "Spades"],
  Ranks = [2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K", "A"],
  [{R, S} || R <- Ranks, S <- Suits].