MySolve[eq_, vars_] := With[{s = Solve[eq, vars]},
    If[Length[s] == 1, s[[1]], Exit[11]]]
