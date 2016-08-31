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
  get_cards(PlayerA, PlayerB, 0, [], []).

%% @doc Gets Cards from Players A and B, then proceeds to battle.
get_cards(A, B, 0, [], []) ->
  A ! {self(), send_one},
  B ! {self(), send_one},
  receive
    {A, no_card} -> win(B, A);
    {A, Card} -> get_cards(A, B, 1, [Card], []);
    {B, no_card} -> win(A, B);
    {B, Card} -> get_cards(A, B, 1, [], [Card])
  end;
get_cards(A, B, 1, ACards, BCards) ->
  receive
    {A, no_card} -> win(B, A);
    {A, Card} -> get_cards(A, B, 2, [Card | ACards], BCards);
    {B, no_card} -> win(A, B);
    {B, Card} -> get_cards(A, B, 2, ACards, [Card | BCards])
  end;
get_cards(A, B, 2, ACards, BCards) -> battle(A, B, ACards, BCards).

%% @doc Gets Cards of two players, pick first of each pile and compares.
battle(A, B, ACards, BCards) ->
  [{AR, _} | _] = ACards, [{BR, _} | _] = BCards,
  Arank = cards:rank(AR),
  Brank = cards:rank(BR),
  compare(A, B, ACards, BCards, Arank, Brank).

%% @doc Gets three additional cards from players in case of draw.
draw(A, B, 0, ACards, BCards) ->
  A ! {self(), draw},
  B ! {self(), draw},
  receive
    {A, []} -> win(B, A);
    {A, Cards} -> draw(A, B, 1, Cards ++ ACards, BCards);
    {B, []} -> win(A, B);
    {B, Cards} -> draw(A, B, 1, ACards, Cards ++ BCards)
  end;
draw(A, B, 1, ACards, BCards) ->
  receive
    {A, []} -> win(B, A);
    {A, Cards} -> draw(A, B, 2, Cards ++ ACards, BCards);
    {B, []} -> win(A, B);
    {B, Cards} -> draw(A, B, 2, ACards, Cards ++ BCards)
  end;
draw(A, B, 2, ACards, BCards) ->
  [{AR, _} | _] = ACards, [{BR, _} | _] = BCards,
  Arank = cards:rank(AR), Brank = cards:rank(BR),
  compare(A, B, ACards, BCards, Arank, Brank).

%% @doc Compares cards and decide which player wins this battle.
compare(A, B, ACards, BCards, Arank, Brank) ->
  if
    Arank > Brank ->
      A ! {self(), {take, ACards ++ BCards}},
      get_cards(A, B, 0, [], []);
    Arank < Brank ->
      B ! {self(), {take, ACards ++ BCards}},
      get_cards(A, B, 0, [], []);
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
    {From, send_one} ->
      case Cards of
        [Card | Left] -> From ! {self(), Card}, player(Left);
        [] -> From ! {self(), no_card}, player([])
      end;
    {From, draw} ->
      case Cards of
        [C1, C2, C3 | Left] -> From ! {self(), [C1, C2, C3]}, player(Left);
        _ -> From ! {self(), Cards}, player([])
      end;
    {_, {take, WinCards}} ->
      player(Cards ++ WinCards);
    {_, win} -> io:format("I ~p win!", [self()]);
    {_, lose} -> io:format("I ~p lose :(", [self()])
  end.