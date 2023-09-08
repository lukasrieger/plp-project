:- use_module(sampler).

0.3333::picked(car) <--- .

picked(goat) :- \+ picked(car).

revealed(goat) :- picked(_).

switch_gets(car) :- picked(goat), revealed(goat).
