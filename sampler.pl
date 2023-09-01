:- module(sampler, [
	load_program/2,
	unload_program/2,
	sample_goal/1,
	op(900, xfx, <---),
	op(950, xfy, ::)
]).

:- dynamic((::)/2).
:- dynamic((<---)/2).

/*
	Load the object program under the given [Program] source and transform its content for future sampling.
*/
load_program(Program, TransformedRules) :-
	consult(Program),
	findall(Weight :: Head <--- Body, sampler: (Weight :: Head <--- Body), Clauses),
	maplist(rewrite_pl_body, Clauses, TransformedRules).

/*
	Given a single PLP clause, transform it into an equivalent standard prolog clause according to the transformation rules detailed in Riguzzi 2013, p. 7.
*/
rewrite_pl_body(Weight :: Head <--- [Term | Rest], TransformedRule) :-
	ListBody = [Term | Rest],
	maplist(collect_free_variables, ListBody, Variables_),
	exclude(is_empty, Variables_, Variables),
	list_to_conjunction(ListBody, Conjunction),
	TransformedRule = (Head :- (!, Conjunction, sample_head([Weight, 1 - Weight], 1, Variables, NH), NH = 0)),
	sampler:assert(TransformedRule).

rewrite_pl_body(Weight :: Head <--- [], TransformedRule) :-
	TransformedRule = (Head :- !, sample_head([Weight, 1 - Weight], 1, [], NH), NH = 0),
	sampler:assert(TransformedRule).

is_empty([]).

/*
	Cleanup environment state (usually after running a sampling process to completion).
 */
unload_program(Program, TransformedRules) :-
	unload_file(Program), maplist(retract_clause, TransformedRules).

retract_clause(Head :- _) :- retractall(Head).

/*
	Transform a list of terms extracted from an object program's clause into a standard prolog conjunction.
*/
list_to_conjunction([Term], Term) :- !.
list_to_conjunction([Term | Rest], ','(Term, Conjunction)) :-
	list_to_conjunction(Rest, Conjunction).

collect_free_variables(Body, FreeVariables) :- term_variables(Body, FreeVariables).

/*
	Generate a sample for a head, given its respective weights.
*/
sample_head(_Weights, RequiredHead, Variables, HeadId) :-
	recorded(samples, (RequiredHead, Variables, HeadId), _), !.

sample_head(Weights, RequiredHead, Variables, HeadId) :-
	sample(Weights, HeadId),
	recorda(samples, (RequiredHead, Variables, HeadId), _).

sample(Weights, HeadId) :-
	random(Prob),
	sample(Weights, 0, 0, Prob, HeadId).

sample([HeadProb|Tail], Index, Prev, Prob, HeadId) :-
	Succ is Index + 1,
	Next is Prev + HeadProb,
	(Prob =< Next ->
		HeadId = Index
	;
		sample(Tail, Succ, Next, Prob, HeadId)
	).


/*
	Assuming a suitable object program has already been prepared via [transform_object_program],
	take a sample of the given [Goal].
*/
sample_goal(Goal) :-
	abolish_all_tables,
	clear_recorded_pl_heads,
	call(Goal).

/*
	Erase all previously recorded sample entries from the database.
*/
clear_recorded_pl_heads :-
	findall(DbReference, recorded(samples, _, DbReference), References),
	erase_all_references(References).

erase_all_references([]).
erase_all_references([Reference | Rest]) :-
	erase(Reference), erase_all_references(Rest).
