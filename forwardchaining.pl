:- op(900, xfx, <---).
:- op(255, xfx, <-).




produces(Facts, Head) :-
    Head <--- sample(X),
    sample(X).

produces(Facts,Head) :-
    % format('produces 1: ~w', Head),
    Head <--- Body,
    % format('produces 1_2: ~w', Head),
    mysubset(Body,Facts).

produces(Facts,Head) :-
    % format('produces 2: ~w', Head),
    member(Head,Facts).


forwardstep(OldFacts,NewFacts) :-
    setof(Head,produces(OldFacts,Head),NewFacts).

forward(Facts) :-
    forward([],Facts).

forward(OldFacts,Facts) :-
    print(OldFacts),
    forwardstep(OldFacts,NewFacts),
    (   NewFacts = OldFacts
    ->  Facts = NewFacts
    ;   forward(NewFacts,Facts)
    ).

mysubset([],_).
mysubset([H|T],List) :-
    member(H,List),
    mysubset(T,List).

