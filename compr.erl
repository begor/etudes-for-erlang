-module(compr).
-export([males_over_40/1, males_or_over_40/1]).

%% @doc Returns names of males which age is over 40.
males_over_40(People) ->
  [Name || {Name, $M, Age} <- People, Age > 40].

%% @doc Returns names of males or females which age is over 40.
males_or_over_40(People) ->
  [Name || {Name, Sex, Age} <- People, Age > 40 orelse Sex =:= $M].
