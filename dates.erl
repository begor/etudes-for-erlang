-module(dates).
-export([date_parts/1, julian/1, test/0]).

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
  {MonthsPassed, _} = lists:split(M - 1, DaysPerMonth),
  DaysUntilCurrentMonth = lists:foldl(fun(Days, Acc) -> Days + Acc end, 0, MonthsPassed),
  case is_leap_year(Y) andalso M > 2 of
    true -> DaysUntilCurrentMonth + D + 1;
    false -> DaysUntilCurrentMonth + D
  end.

%% @doc Simple test suite based on pure pattern-matching assignments.
test() ->
  36 = julian("2012-02-05"),
  36 = julian("2013-02-05"),
  366 = julian("2012-12-31"),
  365 = julian("2013-12-31"),
  60 = julian("1900-03-01"),
  61 = julian("2000-03-01"),
  1 = julian("2013-01-01"),
  ok.

%% @doc Given a year, return true or false depending on whether
%% the year is a leap year.
is_leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0) orelse (Year rem 400 == 0).