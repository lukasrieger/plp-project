:- style_check(-singleton).
:- use_module(sampler).

0.6::epidemic <--- flu(X), cold.
0.3::pandemic <--- flu(X), cold.


0.7::cold <--- [].

flu(david).
flu(robert).

