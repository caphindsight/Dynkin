MySolve[eq_, vars_] := With[{s = Solve[eq, vars]},
    If[Length[s] == 1, s[[1]], Exit[11]]
];

ExpandRootsSingleStep[roots_] := Module[{pairs},
    pairs = Flatten[ Table[{i, j}, {i, roots}, {j, roots}], 1 ];
    DeleteDuplicates [
        roots ~Join~ Table[ Simplify[
            i[[2]] - 2 ((i[[1]].i[[2]]) / (i[[1]].i[[1]])) * i[[1]]
        ], {i, pairs} ]
    ]
];

ExpandRoots[roots_] := Module[{copy, newroots},
    copy = roots;
    newroots = ExpandRootsSingleStep[copy];
    While[ Length[copy] != Length[newroots],
        copy = newroots;
        newroots = ExpandRootsSingleStep[copy];
    ];
    copy
];

PrintRoots[roots_, var_] := With[{n = Length[roots]},
    Table[ Print[var, "[", i, "] = ", roots[[i]], ";"], {i, 1, n} ];
];

