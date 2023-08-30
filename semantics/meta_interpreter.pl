
:- op(900, xfx, <---).
:- op(950, xfy, ::).
:- dynamic((::)/ 2), dynamic((<---) / 2).
:- discontiguous (<---)/2.
:- [plp_objectprogram].
:- [wmc].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Auxiliary predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

memberrev(List,X) :-
    member(X,List).

negate(X,not(X)).

listconstr(A,List,Newlist) :-
    Newlist = [A|List].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prepare for Clarks completion by combining clauses with the same head %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bodies_to_doublelist([Head <--- Body|LPrest], [Head <--- [Body]|SCLPrest]) :-
    \+member(Head <--- _,LPrest),!, bodies_to_doublelist(LPrest,SCLPrest).
bodies_to_doublelist([Head <--- Body|LPrest],SCLP) :-
    bodies_to_doublelist(LPrest,SCLPrest),
    member(Head <--- SCBody,SCLPrest),
    select(Head <--- SCBody,SCLPrest,Head <--- [Body|SCBody],SCLP).
bodies_to_doublelist([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Compute Conjuctive Normal Form %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cnf(Head <--- Body, CNF) :- cnf1(Head <--- Body, CNF1),
			    cnf2(Head <--- Body, CNF2),
			    append(CNF1,CNF2,CNF).

cnf1(Head <--- Body, CNF1) :-
    findall(Clause,maplist(memberrev,[[not(Head)]|Body],Clause),CNF1).

cnf2(Head <--- Body, CNF2) :-
    maplist(maplist(negate),Body,Part),
    maplist(listconstr(Head),Part,CNF2).

compute_cnf([Query|Rest], LP ,[[Query]|CNF]) :-
	compute_cnf(Rest, LP ,CNF),!.

compute_cnf([], [Clause|LP] , CNF) :-
	cnf(Clause, ClauseCNF),
	compute_cnf([], LP, RestCNF),
	append(ClauseCNF,RestCNF,CNF),!.

compute_cnf([], [] , []).

compute_cnf(Query,CNF) :- findall(Clause, (Clause = (_ <--- _), Clause), LP),
			  bodies_to_doublelist(LP,SCLP),
			  compute_cnf(Query,SCLP,CNF),!.
%!  Determine weights %%
weights(Pos,Neg) :-
    findall(Atom-Weight,((Weight::Atom);(Atom <---_, Weight = 1)),Pos1),
    sort(Pos1,Pos),
    findall(Atom-Weight,(((ConWeight::Atom) , Weight is 1 - ConWeight);(Atom <---_,Weight = 1)),Neg1),
    sort(Neg1,Neg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Probability Query %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


prob(Query,Prob) :-
	compute_cnf(Query,CNF),
        weights(Pos,Neg),
	wmc(CNF,Pos,Neg,Prob).












