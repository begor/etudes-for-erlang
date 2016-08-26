-module (dates).
-export ([date_parts/1, julian/1]).

%% @doc Given a string in ISO date format
%% return a list of integers [Year, Month, day]
date_parts(DateString) ->
	[Y, M, D] = re:split(DateString, "-", [{return, list}]),
	
	[element(1, string:to_integer(Y)),
	 element(1, string:to_integer(M)),
	 element(1, string:to_integer(D))].

%% @doc Given a string in ISO format ( "yyyy-mm-dd" ), 
%% it returns the Julian date: the day of the year.
julian(DateString) ->
	DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
	[Y, M, D] = date_parts(DateString),
	julian(Y, M, D, DaysPerMonth, 0).

%% @doc Helper function that recursively accumulates the number of days
%% up to the specified date.
julian(Y, M, D, [_|OtherMonths], Acc) when M < (13 - length(OtherMonths)) ->
	case is_leap_year(Y) andalso M > 2 of
		true -> Acc + D + 1;
		false -> Acc + D
	end;
julian(Y, M, D, [Days|OtherMonths], Acc) -> julian(Y, M, D, OtherMonths, Acc + Days).

%% @doc Given a year, return true or false depending on whether
%% the year is a leap year.
is_leap_year(Year) ->
	(Year rem 4 == 0 andalso Year rem 100 /= 0)
	orelse (Year rem 400 == 0).