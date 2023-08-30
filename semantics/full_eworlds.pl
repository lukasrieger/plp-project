:-  op(1000, xfx, <---).
:-  op(1100, xfx, ::).

:- include(plp_objectprogram).

randEworlds(RandWorlds) :-
	fullEworld(Full),
	findall(World-Prob,
		(   sublist(World,Full), world_prob(World,Prob)),
	        RandWorlds).

fullEworld(Worlds) :-
	setof(X,Y^randEatom(X,Y),Worlds).

randEatom(X,Prob) :-
	(Prob :: X <--- Body),
        call_all(Body).

randEtosses(Tosses) :-
	findall(X-Prob,randEatom(X,Prob),Tosses).

randEatoms(Atoms) :-
	randEtosses(Tosses),
	randEatoms(Atoms,Tosses,[]).

randEatoms(Atoms,[],Atoms).
randEatoms(Atoms,[X-Prob|Tosses],Acc) :-
	\+ member(X-_,Acc),
	!,
	randEatoms(Atoms,Tosses,[X-Prob|Acc]).
randEatoms(Atoms,[X-Prob|Tosses],Acc) :-
	member(X-Probold,Acc),
	!,
	Probnew is 1 - (1 - Prob)*(1-Probold),
	select(X-Probold,Acc,X-Probnew,Accnew),
	randEatoms(Atoms,Tosses,Accnew).

world_prob(World,Prob) :-
	randEatoms(RandAtoms),
	world_prob(World,Prob,1,RandAtoms).

world_prob(_,Acc,Acc,[]).
world_prob(World,Prob,Acc,[Atom-HeadProb|RandAtoms]) :-
	memberchk(Atom,World),
	Accnew is Acc*HeadProb,
	world_prob(World,Prob,Accnew,RandAtoms).
world_prob(World,Prob,Acc,[Atom-HeadProb|RandAtoms]) :-
	\+memberchk(Atom,World),
	Accnew is Acc*(1-HeadProb),
	world_prob(World,Prob,Accnew,RandAtoms).

sublist(List,List).
sublist(Sublist,[H|T]) :-
	sublist_aux(T,H,Sublist).

sublist_aux(Sublist,_,Sublist).
sublist_aux([H|T],_,Sublist) :-
	sublist_aux(T,H,Sublist).
sublist_aux([H|T],X,[X|Sublist]) :-
	sublist_aux(T,H,Sublist).

call_all([]).
call_all([H|T]) :-
	call(H),
	call_all(T).









