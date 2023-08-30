:-  op(900, xfx, <---).
:-  op(950, xfx, ::).


:- dynamic sortpred/1.
:- dynamic (<---)/2.
:- dynamic (::)/2.

convert(Prob :: Head <--- Body,Reference,Clause,Pfact) :-
    freevars(Head <--- Body,Vars),
    separate_sorts(Body,Sorts,Nonsorts),
    Fact =.. [Reference|Vars],
    Pfact = (Prob :: Fact <--- Sorts),
    NewBody = [Fact|Nonsorts],
    Clause = (Head <--- NewBody).

convert_all :-
    clause(Prob :: Head <--- Body,true,Reference),
    \+only_sorts(Body),
    convert(Prob :: Head <--- Body,Reference,Clause,Pfact),
    assert(Clause),
    assert(Pfact),
    print(Pfact),
    erase(Reference),
    fail.
convert_all.

freevars(Term, VarList) :-
    '$free_variable_set'([]^Term, _Goal, Vars),
    Vars =.. [_|VarList].

separate_sorts([],[],[]).
separate_sorts([First|Body],[First|Sorts],Nonsorts) :-
    functor(First,Name,1),
    sortpred(Name),
    separate_sorts(Body,Sorts,Nonsorts).
separate_sorts([First|Body],Sorts,[First|Nonsorts]) :-
    functor(First,Name,_),
    \+sortpred(Name),
    separate_sorts(Body,Sorts,Nonsorts).

only_sorts([]).
only_sorts([First|Body]) :-
    functor(First,Name,1),
    sortpred(Name),
    only_sorts(Body).
















