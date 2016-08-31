-module(war).

%% API
-export([player/1, start/0]).

%%% GAME

%% @doc Starts a game of war:
%% 1. Shuffles deck
%% 2. Spawns players
%% 3. Give player a half of a deck
%% 4. Proceeds to first turn
start() ->
  Deck = cards:shuffle(cards:make_small_deck()),
  {PlayerACards, PlayerBCards} = lists:split(length(Deck) div 2, Deck),
  PlayerA = spawn(?MODULE, player, [PlayerACards]),
  PlayerB = spawn(?MODULE, player, [PlayerBCards]),
  turn(PlayerA, PlayerB, 0, [], []).

%% @doc Gets Cards from Players A and B, then proceeds to battle.
turn(A, B,  0, ACards, BCards) ->
  A ! {self(), turn},
  B ! {self(), turn},
  receive
    {A, Cards} -> turn(A, B, 1, Cards ++ ACards, BCards);
    {B, Cards} -> turn(A, B, 1, ACards, Cards ++ BCards)
  end;
turn(A, B, 1, ACards, BCards) ->
  receive
    {A, Cards} -> turn(A, B, 2, Cards ++ ACards, BCards);
    {B, Cards} -> turn(A, B, 2, ACards, Cards ++ BCards)
  end;
turn(A, B, 2, ACards, BCards) -> battle(A, B, ACards, BCards).

%% @doc Gets three additional cards from players in case of draw.
draw(A, B, 0, ACards, BCards) ->
  A ! {self(), draw},
  B ! {self(), draw},
  receive
    {A, Cards} -> draw(A, B, 1, Cards ++ ACards, BCards);
    {B, Cards} -> draw(A, B, 1, ACards, Cards ++ BCards)
  end;
draw(A, B, 1, ACards, BCards) ->
  receive
    {A, Cards} -> draw(A, B, 2, Cards ++ ACards, BCards);
    {B, Cards} -> draw(A, B, 2, ACards, Cards ++ BCards)
  end;
draw(A, B, 2, ACards, BCards) ->
  battle(A, B, ACards, BCards).

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
      A ! {self(), {take, ACards ++ BCards}},
      turn(A, B, 0, [], []);
    ATopCardRank < BTopCardRank ->
      B ! {self(), {take, ACards ++ BCards}},
      turn(A, B, 0, [], []);
    true ->
      draw(A, B, 0, ACards, BCards)
  end.

%% @doc Sends messages about winning and loosing.
win(X, Y) ->
  X ! {self(), win}, Y ! {self(), lose}.

%%% PLAYERS

%% @doc Player actor.
player(Cards) ->
  io:format("I am ~p, and I have ~p~n", [self(), Cards]),
  receive
    {From, turn} ->
      case Cards of
        [Card | Left] -> From ! {self(), [Card]}, player(Left);
        [] -> From ! {self(), []}, player([])
      end;
    {From, draw} ->
      case Cards of
        [C1, C2, C3 | Left] -> From ! {self(), [C1, C2, C3]}, player(Left);
        _ -> From ! {self(), Cards}, player([])
      end;
    {_, {take, WinCards}} ->
      player(Cards ++ WinCards);
    {_, win} -> io:format("I ~p win!~n", [self()]);
    {_, lose} -> io:format("I ~p lose :(~n", [self()])
  end.