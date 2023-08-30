
:- op(1200, xfy, ::).
:- discontiguous wmc_normalised_theory/4.
:- table connected/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Extracts weights */
weight(Literal,_,NegWeights,Weight) :-
    Literal = not(Atom),
    member(Atom-Weight,NegWeights).
weight(Atom, PosWeights,_,Weight) :-
    Atom \= not(_),
    member(Atom-Weight,PosWeights).

/* For a literal A, this replaces builds T_A as used in Unit Propagation and Case Analysis  */
build_TA([Clause|T],Literal,[ClauseTA|TA]) :-
	Literal \= not(_),
	replace(Clause,Literal,true,Clause1),
	replace(Clause1,not(Literal),false,ClauseTA),
	build_TA(T,Literal,TA),!.
build_TA([Clause|T],Literal,[ClauseTA|TA]) :-
	Literal = not(Atom),
	replace(Clause,Atom,false,Clause1),
	replace(Clause1,Literal,true,ClauseTA),
	build_TA(T,Literal,TA),!.
build_TA([],_,[]).

/* Replacing all occurences of X with Y */
replace([H|List], X , Y, [RH|RList]) :-
	H = X,!,RH=Y,replace(List, X , Y, RList).
replace([H|List], X , Y, [RH|RList]) :-
	H \= X,!,RH=H,replace(List, X , Y, RList).
replace([], _ , _, []).

/* Some simple propositional equivalences */
normalise([Clause|T],[NClause|NT]) :- normalise_clause(Clause,NClause), normalise(T,NT),!.
normalise([],[]).

normalise_clause([Literal|Clause],NClause) :-
	Literal = not(not(Literal1)),!,
	normalise_clause([Literal1|Clause],NClause).
normalise_clause([Literal|Clause],[NLiteral|NClause]) :-
	Literal = not(true), !, NLiteral = false,
	normalise_clause(Clause,NClause).
normalise_clause([Literal|Clause],[NLiteral|NClause]) :-
	Literal = not(false),!, NLiteral = true,
	normalise_clause(Clause,NClause).
normalise_clause([Literal|Clause],[Literal|NClause]) :-
    normalise_clause(Clause,NClause).
normalise_clause([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Weighted Model Counting of Normalised Clauses %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trivial cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wmc_normalised_theory([],_,_,1) :- !.
wmc_normalised_theory(T,_,_,0) :- member([],T),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simplify %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wmc_normalised_theory(T,PosWeights,NegWeights, WMC) :-
	member(Clause,T),
	member(true,Clause),
        select(Clause,T,Rest),
        !,
	forgottenAtoms(Clause,Rest,ForgottenLiterals),
	compute_weight_of_forgottenAtoms(ForgottenLiterals,PosWeights,NegWeights,WeightForgottenLiterals),
	wmc_normalised_theory(Rest,PosWeights,NegWeights, WMCRest),
	WMC is WeightForgottenLiterals*WMCRest.
wmc_normalised_theory(T,PosWeights,NegWeights, WMC) :-
	member(Clause,T),
	member(false,Clause),
        select(Clause,T,Rest),
        !,
	delete(Clause,false,NewClause),
	wmc_normalised_theory([NewClause|Rest],PosWeights,NegWeights, WMC),!.


% forgotten Literals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forgottenAtoms([Literal|Clause],T,ForgottenLiterals) :-
	Literal = true,!,
	forgottenAtoms(Clause,T,ForgottenLiterals).
forgottenAtoms([Literal|Clause],T,ForgottenLiterals) :-
	Literal = false,!,
	forgottenAtoms(Clause,T,ForgottenLiterals).
forgottenAtoms([Literal|Clause],T,[Atom|ForgottenLiterals]) :-
        Literal = not(Atom),
	forget_check(Atom,T),!,
	forgottenAtoms(Clause,T,ForgottenLiterals).
forgottenAtoms([Atom|Clause],T,[Atom|ForgottenLiterals]) :-
        Atom \= not(_),
        forget_check(Atom,T),!,
	forgottenAtoms(Clause,T,ForgottenLiterals).
forgottenAtoms([_|Clause],T,ForgottenLiterals) :-
	forgottenAtoms(Clause,T,ForgottenLiterals).
forgottenAtoms([],_,[]).

forget_check(Literal,[Clause|T]) :-
    \+occurschk(Literal,Clause),
    forget_check(Literal,T).
forget_check(_,[]).

occurschk(Atom,[Atom|_]) :- !.
occurschk(Atom,[not(Atom)|_]) :- !.
occurschk(Atom,[_|Clause]) :- occurschk(Atom,Clause).

compute_weight_of_forgottenAtoms([Atom|ForgottenAtoms],PosWeights,NegWeights, Weight) :-
	weight(Atom,PosWeights,NegWeights, WeightLiteral),
	weight(not(Atom),PosWeights,NegWeights, WeightNegLiteral),
	compute_weight_of_forgottenAtoms(ForgottenAtoms,PosWeights,NegWeights, WeightForgottenLiterals),
	Weight is (WeightLiteral + WeightNegLiteral)*WeightForgottenLiterals.
compute_weight_of_forgottenAtoms([],_,_, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unit propagation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wmc_normalised_theory(T,PosWeights,NegWeights,WMC) :-
    member([Literal],T),
    select([Literal],T,Rest),
    build_TA(Rest,Literal,TA),
    weight(Literal,PosWeights,NegWeights,WeightLiteral),
    wmc_normalised_theory(TA, PosWeights,NegWeights,WMCTA),!,
    WMC is WeightLiteral*WMCTA.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Decomposition%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%wmc_normalised_theory(T,PosWeights,NegWeights, WMC) :-
%    subseq(T,T1,T2),
%    T1\=[],
%    T2\=[],
%    no_shared_vars(T1,T2),
%    !,
%    wmc_normalised_theory(T1,PosWeights,NegWeights, WMC1),
%    wmc_normalised_theory(T2,PosWeights,NegWeights, WMC2),
%    WMC is WMC1 * WMC2.

wmc_normalised_theory([Clause|T],PosWeights,NegWeights, WMC) :-
    setof(Clause2,connected(Clause,Clause2,[Clause|T]),T1),
    subtract(T,T1,T2),
    T2 \=[],
    wmc_normalised_theory(T1,PosWeights,NegWeights, WMC1),
    wmc_normalised_theory(T2,PosWeights,NegWeights, WMC2),
    WMC is WMC1 * WMC2.


shared_variables(Clause1,Clause2,T) :-
    member(Clause1,T),
    member(Clause2,T),
    occurs(A,Clause1),
    occurs(A,Clause2).
connected(Clause1,Clause2,T) :-
    shared_variables(Clause1,Clause2,T).
connected(Clause1,Clause3,T) :-
    shared_variables(Clause1,Clause2,T),
    connected(Clause2,Clause3,T).

%no_shared_vars([],_).
%no_shared_vars([Clause|Rest],T2) :-
%    no_shared_vars_ct(Clause,T2),
%    no_shared_vars(Rest,T2).
%
%no_shared_vars_ct(_,[]).
%no_shared_vars_ct(Clause,[Clause2|Rest]) :-
%    \+ (occurs(Atom,Clause),
%       occurs(Atom,Clause2)),
%    no_shared_vars_ct(Clause,Rest).

occurs(Atom,[Atom|_]).
occurs(Atom,[not(Atom)|_]).
occurs(Atom,[_|Clause]) :- occurs(Atom,Clause).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% case analysis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wmc_normalised_theory(T,PosWeights,NegWeights, Weight) :-
	T = [Clause|_],
	Clause = [Atom|_],
        Atom \= (not(_)),
	build_TA(T,Atom,TA),
	build_TA(T,not(Atom),TnegA),
	weight(Atom,PosWeights,NegWeights,WeightA),
	weight(not(Atom),PosWeights,NegWeights,WeightnegA),
	wmc_normalised_theory(TA,PosWeights,NegWeights, WeightTA),
	wmc_normalised_theory(TnegA,PosWeights,NegWeights, WeightTnegA),!,
	Weight is (WeightA*WeightTA + WeightnegA*WeightTnegA).

wmc_normalised_theory(T,PosWeights,NegWeights, Weight) :-
	T = [Clause|_],
	Clause = [not(Atom)|_],
        build_TA(T,Atom,TA),
	build_TA(T,not(Atom),TnegA),
	weight(Atom,PosWeights,NegWeights,WeightA),
	weight(not(Atom),PosWeights,NegWeights,WeightnegA),
	wmc_normalised_theory(TA,PosWeights,NegWeights, WeightTA),
	wmc_normalised_theory(TnegA,PosWeights,NegWeights, WeightTnegA),!,
	Weight is (WeightA*WeightTA + WeightnegA*WeightTnegA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WMC of general clauses %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wmc(Theory,PosWeights,NegWeights, Weight) :-
	normalise(Theory,NTheory),
	wmc_normalised_theory(NTheory,PosWeights,NegWeights, Weight).



