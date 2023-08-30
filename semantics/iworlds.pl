:-  op(900, xfx, <---).
:-  op(950, xfx, ::).

:- include(plp_objectprogram).
:- include(full_eworlds).
:- include(forwardchaining).

randIworlds(RandWorlds) :-
    randEworlds(RandEworlds),
    findall(World-Prob,(member(Eworld-Prob,RandEworlds), forward(Eworld,World)),RandWorlds).

marginal(Query,Prob) :-
    is_list(Query),
    complex_marginal(Query,Prob).

marginal(Query,Prob) :-
    \+is_list(Query),
    simple_marginal(Query,Prob).


simple_marginal(Query,QProb) :-
    randEworlds(RandEworlds),
    aggregate_all(sum(Prob),
                 (member(Eworld-Prob,RandEworlds),
                  forward(Eworld,World),
                  memberchk(Query,World)),
                 QProb).

complex_marginal(Query,QProb) :-
    randEworlds(RandEworlds),
    aggregate_all(sum(Prob),
                 (member(Eworld-Prob,RandEworlds),
                  forward(Eworld,World),
                  mysubset(Query,World)),
                 QProb).



/* Forward chaining from extensional to intensional worlds */



