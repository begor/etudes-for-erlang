-module(ask_area).
-export([area/0]).

%%% PUBLIC API

%% @doc Ask user to prompt first letter of a shape,
%% then continue to calculating an area of that shape,
%% asking its dimensions.
area() ->
  [Answer | _] = io:get_line("R)ectangle, T)riangle, or E)llipse > "),
  Shape = char_to_shape(Answer),

  case Shape of
    unknown -> {X, Y} = {error, "Unknown shape " ++ [Answer]};
    rectangle -> {X, Y} = get_dimensions("width", "height");
    ellipse -> {X, Y} = get_dimensions("base", "height");
    triangle -> {X, Y} = get_dimensions("major axis", "minor axis")
  end,

  calculate(Shape, X, Y).

%%% HELPER FUNCIONS

%% @doc Given a Char, return an atom, corresponding to
%% that Char or unknown.
char_to_shape(Char) ->
  case Char of
    $R -> rectangle;
    $r -> rectangle;
    $E -> ellipse;
    $e -> ellipse;
    $T -> triangle;
    $t -> triangle;
    _ -> unknown
  end.

%% @doc Get two dimensions, named Prompt1 and Prompt2.
get_dimensions(Prompt1, Prompt2) ->
  {get_number(Prompt1), get_number(Prompt2)}.

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

%% @doc Calculate an area for a given Shape and Dimensions.
calculate(unknown, _, Err) -> io:format("~s~n", [Err]);
calculate(_, error, _) -> io:format("Error in first number.~n");
calculate(_, _, error) -> io:format("Error in second number.~n");
calculate(_, A, B) when A < 0; B < 0 ->
  io:format("Both numbers must be greater than or equal to zero~n");
calculate(Shape, A, B) -> geom:area(Shape, A, B).