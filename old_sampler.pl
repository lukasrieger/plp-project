:- op(900, xfx, <---).
:- op(950, xfy, ::).
:- dynamic((::)/ 2), dynamic((<---) / 2).
:- discontiguous (<---)/2.


:-include(input_objectprogram).
:-include(converter).
:-include(forwardchaining).


transform_input(Transformed) :- 
    findall(Weight :: Head <--- Body ,Weight ::  Head <--- Body, Clauses), 
    maplist(rewrite, Clauses, Transformed).



collect(Body, R) :- term_variables(Body, R).
empty([]).

list_to_conj([H], H) :- !.
list_to_conj([H | T], ','(H, Conj)) :-
    list_to_conj(T, Conj).

rewrite(Weight :: Head <--- [H|T], WithSample) :- 
    Body = [H|T],
    maplist(collect, Body, Vars),
    exclude(empty, Vars, VarsN),
    list_to_conj(Body, Conj),
    Clause = (Head :- (!, Conj, sample(Weight, VarsN, Head))),
    WithSample = Clause,
    assert(Clause). 


rewrite(Weight :: Head <--- [], Transformed) :- 
    Clause = (Head :- !, sample(Weight, [], Head)),
    assert(Clause),
    Transformed = Clause.



sample(Weight, Vars, Head) :- 
    recorded(samples, (Head, Vars), _), !.
sample(Weight, Vars, Head) :-
    sample_(Weight), recorda(samples, (Head,Vars)).

sample_(Par) :- random(Prob), Prob < Par.

% flu(David) : [] <--- [flu(David)]
coll(Facts) :- findall(Fact, ground(Fact), Facts), print(Facts).
rewrite_facts(Rewritten) :- coll(Facts), print(Facts), maplist(rewrite_fact, Facts, Rewritten).
rewrite_fact(Fact, Transformed) :- Transformed = ([] <--- [Fact]).



collectFacts([], Return).

collectFacts([H|T], Return) :- \+ ground(H),
collectFacts(T, Return).

collectFacts([H|T], Return) :- ground(H),
Return = [Return|H],
print(H),
collectFacts(T, Return).

isFact(H) :- Head <--- [Body | sample(Weight)].
isFact(H) :- Fact <--- sample(Weight).


transform_program(R) :- transform_input(R). %findall(Query,  (member(Query, NewW), forward([flu(David), flu(Robert)], NewW)), Result), R = Result.


take_sample(Goal) :-
    abolish_all_tables,
    findall(Ref, recorded(samples,_,Ref), L),
    eraseall(L),
    transform_program(R),
    call(Goal). 

eraseall([]).
eraseall([Reference| T]) :- erase(Reference) , eraseall(T). 

% transform_input([(pandemic : 0.3) <--- (flu(X), cold), (epidemic : 0.6) <--- (flu(X), cold), (cold : 0.7), flu(Robert), flu(David)]).


% Call via: 
% transform_input([(0.3 :: pandemic) <--- [flu(X), cold], (0.6 :: epidemic ) <--- [flu(X), cold], (0.7 :: cold), flu(Robert), flu(David)], Result).
% Result = [(pandemic<---(flu(X), cold), sample(0.3)), (epidemic<---(flu(X), cold), sample(0.6)), cold<---sample(0.7), flu(Robert), flu(David)]

% findall(World-Prob,(member(Eworld-Prob,RandEworlds), forward(Eworld,World)),RandWorlds).
sample_program(Query, N, R) :- transform_input(Transformed), collectFacts([(pandemic : 0.3) <--- (flu(X), cold), (epidemic : 0.6) <--- (flu(X), cold),  (cold : 0.7), flu(David)], Rewritten), print('Rewritten'),print(Rewritten), forward(Rewritten, Facts), print(Facts). %findall(Query,  (member(Query, NewW), forward(OldW, NewW)), Result), R = Result.



% rewrite(Weight :: Head <--- Body, Result) :- 
%     maplist(collect, Body, VarS),
%     Result = [Head <--- [Body, sample(Weight)], VarS].#








call_all([]).
call_all([H|T]) :-
	call(H),
	call_all(T).



%! Code from Paper below


% epidemic : 0.6 :- flu(X), cold.
% pandemic : 0.3 :- flu(X), cold. 
% cold : 0.7.
% flu(david).
% flu(robert).



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




% pandemic<---[<clause>(0000019dfbfed260)(_592),cold]
% 0.3::<clause>(0000019dfbfed260)(_592)<---[flu(_592)]
% epidemic<---[<clause>(0000019dfbfed5a0)(_592),cold]
% 0.6::<clause>(0000019dfbfed5a0)(_592)<---[flu(_592)]
% [
%     cold<---sample(0.7),
% <clause>(0000019dfbfed260)(_658)<---[[flu(_658)]|sample(0.3)],
% <clause>(0000019dfbfed5a0)(_620)<---[[flu(_620)]|sample(0.6)]
% ]
