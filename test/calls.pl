:- use_module(test).

?- mc_test('examples/calls', calls(mary), 0.28).
?- mc_test('examples/calls', calls(_), 0.352).
?- mc_test('examples/calls', calls(_), 0.352, [per_grounding(1)]).
