:- use_module(test).

?- mc_test('examples/alarm', alarm(true), standard, 0.3).
?- mc_test('examples/alarm', alarm(false), standard, 0.7).
?- mc_test('examples/alarm', alarm(true), (gibbs, 1), 0.3).
?- mc_test('examples/alarm', alarm(false), (gibbs, 1), 0.7).
