-module(war).

%% API
-export([play_game/0, player/1, dealer/0]).

%% @doc Spawns dealer process.
play_game() ->
  spawn(?MODULE, dealer, []).

%% @doc Starts a game of war:
%% 1. Shuffles deck
%% 2. Spawns players
%% 3. Give player a half of a deck
%% 4. Proceeds to first turn
dealer() ->
  Deck = cards:shuffle(cards:make_small_deck()),
  {PlayerACards, PlayerBCards} = lists:split(length(Deck) div 2, Deck),
  PlayerA = spawn(?MODULE, player, [PlayerACards]),
  PlayerB = spawn(?MODULE, player, [PlayerBCards]),
  turn(PlayerA, PlayerB, turn, 0, [], []).

%% @doc Gets Cards from Players A and B, then proceeds to battle.
turn(A, B, Type, 0, ACards, BCards) ->
  A ! {self(), Type},
  B ! {self(), Type},
  receive
    {A, Cards} -> turn(A, B, Type, 1, Cards ++ ACards, BCards);
    {B, Cards} -> turn(A, B, Type, 1, ACards, Cards ++ BCards)
  end;
turn(A, B, Type, 1, ACards, BCards) ->
  receive
    {A, Cards} -> turn(A, B, Type, 2, Cards ++ ACards, BCards);
    {B, Cards} -> turn(A, B, Type, 2, ACards, Cards ++ BCards)
  end;
turn(A, B, _, 2, ACards, BCards) -> battle(A, B, ACards, BCards).

%% @doc Gets Cards of two players, pick first of each pile and compares.
battle(A, B, [], _BCards) -> win(B, A);
battle(A, B, _ACards, []) -> win(A, B);
battle(A, B, ACards, BCards) -> compare(A, B, ACards, BCards).

%% @doc Compares cards and decide which player wins this battle.
compare(A, B, ACards, BCards) ->
  [ATopCard|_] = ACards, [BTopCard|_] = BCards,
  {{ATopCardRank, _}, {BTopCardRank, _}} = {ATopCard, BTopCard},
  if
    ATopCardRank > BTopCardRank ->
      io:format("A takes~n"),
      A ! {self(), {take, ACards ++ BCards}},
      turn(A, B, turn, 0, [], []);
    ATopCardRank < BTopCardRank ->
      io:format("B takes~n"),
      B ! {self(), {take, ACards ++ BCards}},
      turn(A, B, turn, 0, [], []);
    true ->
      io:format("Draw~n"),
      turn(A, B, draw, 0, ACards, BCards)
  end.

%% @doc End game function.
%% Sends messages about win/lose and kills processes.
win(X, Y) ->
  X ! {self(), win}, Y ! {self(), lose},
  exit(X, kill), exit(Y, kill),
  io:format("Game finished!~n").

%%% PLAYERS

%% @doc Player actor.
player(Cards) ->
  receive
    {From, turn} ->
      case Cards of
        [Card | Left] -> From ! {self(), [Card]}, player(Left);
        [] -> io:format("Should be it!"), From ! {self(), []}, player([])
      end;
    {From, draw} ->
      case Cards of
        [C1, C2, C3 | Left] -> From ! {self(), [C3, C2, C1]}, player(Left);
        _ -> From ! {self(), Cards}, player([])
      end;
    {_, {take, WinCards}} ->
      player(Cards ++ WinCards);
    {_, win} -> io:format("I ~p win!~n", [self()]);
    {_, lose} -> io:format("I ~p lose :(~n", [self()])
  end.