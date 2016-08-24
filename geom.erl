%% @doc Functions for calculating geometrical formulas.
-module (geom).
-export ([area/1, area/2]).


%% @doc Calculates the area of a rectangle, given
%% its two sides.
area(X, Y) -> X * Y.


%% @doc Calculates area for a given shape 
%% (provided as a triple).
area({Shape, X, Y}) -> area(Shape, X, Y).


%% @doc Calculates area for a given shape 
%% (provided as a first atom argument).
area(Shape, X, Y) when X >= 0, Y >= 0 ->
    case Shape of
        rectangle -> X * Y;
        triangle -> (X * Y) / 2.0;
        ellipse -> X * Y * math:pi()
    end.