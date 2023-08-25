:- op(900, xfx, <---).
% :- op(950, xfy, ::).
% :- dynamic((::)/ 2), dynamic((<---) / 2).
:- discontiguous (<---)/2.

% epidemic : 0.6 :- flu(X), cold.
% pandemic : 0.3 :- flu(X), cold.

% cold : 0.7.


% flu(david).
% flu(robert).



% dosomething([]).
% dosomething([H|T], Query) :- process(H, Query), dosomething(T).

% process(H, Query) :- H == Query, calc(H).
%  %calc(H) :- .

% input(Query, Program) :- transform_input(Program, Result), interpret(Result, Query).

% interpret(A, B) :- _.


transform_input(Program, Transformed) :- maplist(rewrite, Program, Result), Transformed = Result.



% epidemic : 0.6 <--- flu(x), cold.  ------------> epidemic <--- flu(x), cold, sample(0.6)
rewrite(Head:Weight <--- Body, Transformed) :- Transformed = (Head <--- Body, sample(Weight)).
rewrite(Head <--- Body, Transformed) :- Transformed = (Head <--- Body).
rewrite(Fact : Weight, Transformed) :- Transformed = (Fact <--- sample(Weight)).
rewrite(Fact, Transformed) :- Transformed = Fact.


sample(Par) :- random(Prob), Prob < Par.

% transform_input([(pandemic : 0.3) <--- (flu(X), cold), (epidemic : 0.6) <--- (flu(X), cold), (cold : 0.7), flu(Robert), flu(David)]).


%! Code from Paper below

% sample(ParList, HeadId) :-
% random(Prob),
% sample(ParList, 0, 0, Prob, HeadId).
% sample([HeadProb|Tail], Index, Prev, Prob, HeadId) :- Succ is Index + 1,
% Next is Prev + HeadProb,
% (Prob =< Next ->
% HeadId = Index
% ;
% sample(Tail, Succ, Next, Prob, HeadId)
% ).


