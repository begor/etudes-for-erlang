-module(teeth).
-export([alert/1]).

%% @doc Create a list of tooth numbers that require attention.
alert([]) -> [];
alert([[0] | Teeths]) -> alert(Teeths);
alert(Teeths) ->
  [Teeth | OtherTeeths] = Teeths,
  Position = 32 - length(OtherTeeths),
  case stats:maximum(Teeth) >= 4 of
    true -> [Position] ++ alert(OtherTeeths);
    false -> alert(OtherTeeths)
  end.