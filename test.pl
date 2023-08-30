:-  op(900, xfx, <---).
:-  op(950, xfx, ::).

:- include(input_objectprogram).
:- include(eworlds).
:- include(converter).

randWorlds(Result) :- randEworlds(RandEworlds), print(RandEworlds), Result = RandEworlds.

edge(rtz).
edge(a).
edge(y).

rewrite(Weight :: Head <--- Body, Result) :- 
    maplist(collect, Body, VarS),
    Result = [Head <--- Body, VarS].

collect(Body, R) :-
    term_variables(Body, Vars),
    findall(Vars, Body, Solutions),
    R = (Vars - Solutions).


test(R, Goal) :- 
    term_variables(Goal, Vars),
    findall(Vars, Goal, R).



