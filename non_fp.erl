-module (non_fp).
-export ([generate_teeth/2, test_teeth/0]).

%% @doc Generate a list of lists, six numbers per tooth, giving random
%% pocket depths and string containint "T"s for present teeth and
%% "F"s for missing ones.
generate_teeth(Teeths, Prob) ->
	random:seed(now()),
	generate_teeth(Teeths, Prob, []).

%% @doc Tail recursive generator of teeths.
%% [0] denotes missing tooth.
generate_teeth([], _, Acc) ->
	lists:reverse(Acc);
generate_teeth([$F|T], Prob, Acc) ->
	generate_teeth(T, Prob, [[0]|Acc]);
generate_teeth([$T|T], Prob, Acc) ->
	generate_teeth(T, Prob, [generate_tooth(Prob)|Acc]).

%% @doc Generate one teeth's pockets.
generate_tooth(Prob) ->
	Pockets = 6,
	BaseDepth = case random:uniform() < Prob of
								true -> 2;
	  						false -> 3
							end,
	generate_tooth(BaseDepth, Pockets, []).

%% @doc Generate one teeth's pockets given Base depth.
generate_tooth(_, 0, Acc) ->
	Acc;
generate_tooth(Base, Left, Acc) ->
	RandomDepth = Base + random:uniform(3) - 2,
	generate_tooth(Base, Left - 1, [RandomDepth|Acc]).

%% @doc Simple testing functions.
test_teeth() ->
	TList = "FTTTTFFTTTTTTTTFTTTTTTTTFFTTTTTT",
	N = generate_teeth(TList, 0.5),
	print_tooth(N).
print_tooth([]) -> ok;
print_tooth([H|T]) ->
	io:format("~p~n", [H]),
	print_tooth(T).
