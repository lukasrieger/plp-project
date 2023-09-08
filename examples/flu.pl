:- use_module(sampler).

0.6::epidemic <--- flu(_), cold.
0.3::pandemic <--- flu(_), cold.

0.7::cold <--- .

flu(david).
flu(robert).
