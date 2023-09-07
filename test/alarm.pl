:- use_module(test).

?- mc_test('examples/alarm', alarm(true), 0.3).
?- mc_test('examples/alarm', alarm(false), 0.7).
