:- use_module(test).

?- mc_test('examples/calls', calls(mary), standard, 0.28).
?- mc_test('examples/calls', calls(mary), (gibbs, 1), 0.28).
