:- use_module(test).

?- mc_test('examples/flu', epidemic, standard, 0.588).
?- mc_test('examples/flu', epidemic, (gibbs, 1), 0.588).
