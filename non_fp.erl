-module (non_fp).
-export ([generate_teeth/2]).


generate_teeth(GoodTeeth, Prob) -> generate_teeth(GoodTeeth, Prob, []).


generate_teeth([], Prob, Acc) -> lists:reverse(Acc);
generate_teeth([$F|T], Prob, Acc) -> generate_teeth(T, Prob, [0|Acc]);
generate_teeth([$T|T], Prob, Acc) -> generate_teeth(T, Prob, [generate_tooth(Prob)|Acc]).


generate_tooth(Prob) ->
	case random:uniform() < Prob of
		true -> generate_tooth(2, 6, []);
	    false -> generate_tooth(3, 6, [])
	end.


generate_tooth(_, 0, Acc) -> Acc;
generate_tooth(Base, Left, Acc) -> 
	generate_tooth(Base, Left - 1, [(Base + random:uniform(3) - 2)|Acc]).
