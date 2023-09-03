:- style_check(-singleton).
:- use_module(sampler).

0.1::calls(X) <--- [person(X)].
0.2::calls(mary) <--- [].

person(mary).
person(george).
