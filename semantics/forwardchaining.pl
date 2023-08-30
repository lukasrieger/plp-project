:- op(255, xfx, <-).
:- op(900, xfx, <---).

:- dynamic (<---)/2.


produces(Facts,Head) :-
    Head <--- Body,
    mysubset(Body,Facts).

produces(Facts,Head) :-
    member(Head,Facts).

forwardstep(OldFacts,NewFacts) :-
    setof(Head,produces(OldFacts,Head),NewFacts).

forward(Facts) :-
    forward([],Facts).

forward(OldFacts,Facts) :-
    forwardstep(OldFacts,NewFacts),
    (   NewFacts = OldFacts
    ->  Facts = NewFacts
    ;   forward(NewFacts,Facts)
    ).

mysubset([],_).
mysubset([H|T],List) :-
    member(H,List),
    mysubset(T,List).
