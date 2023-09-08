:- style_check(-singleton).
:- use_module(sampler).

0.5::chilly; 0.5::freezing <--- cold.

0.6::epidemic; 0.2::pandemic <--- flu(X), cold.

0.7::cold <--- .

flu(robert).
flu(david).
