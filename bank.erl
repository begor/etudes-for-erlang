-module(bank).

%% API
-export([account/1]).


account(Balance) ->
  [Answer | _] = io:get_line("D)eposit, W)ithdraw, B)alance, Q)uit> "),
  NewBalance = handle_request(Answer, Balance),
  io:format("~n New balance is ~p ~n", [NewBalance]),
  account(NewBalance).

%% @doc Handles deposit action.
handle_request($D, Balance) -> Balance + get_number("Amount to deposit: ");
handle_request($d, Balance) -> Balance + get_number("Amount to deposit: ");
handle_request($W, Balance) -> Balance - get_number("Amount to withdraw: ");
handle_request($w, Balance) -> Balance - get_number("Amount to withdraw: ").

%% @doc Given a Prompt, asks user to input.
%% Then firstly try to convert input to float, secondly - to int.
get_number(Prompt) ->
  Str = io:get_line("Enter " ++ [Prompt] ++ " > "),
  {Number, _} = string:to_float(Str),
  case Number of
    error -> {N, _} = string:to_integer(Str);
    _ -> N = Number
  end,
  N.