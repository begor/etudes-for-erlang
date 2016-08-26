-module (dates).
-export ([date_parts/1]).

%% @doc Given a string in ISO date format
%% return a list of integers [Year, Month, day]
date_parts(DateString) ->
	[Y, M, D] = re:split(DateString, "-", [{return, list}]),
	
	[element(1, string:to_integer(Y)),
	 element(1, string:to_integer(M)),
	 element(1, string:to_integer(D))].