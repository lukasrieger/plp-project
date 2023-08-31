:- module(montecarlo, [montecarlo/3, montecarlo/4, montecarlo/5]).

:- op(900, xfx, <---).
:- op(950, xfy, ::).
:- dynamic((::)/ 2), dynamic((<---) / 2).
:- discontiguous (<---)/2.

:- use_module(sampler).

montecarlo(Program, Query, Probability) :-
	montecarlo(Program, Query, 0.02, Probability).
montecarlo(Program, Query, Threshold, Probability) :-
	montecarlo(Program, Query, Threshold, 500, Probability).
montecarlo(Program, Query, Threshold, BatchSize, Probability) :-
	transform(Program, TransformedRules),
	take_samples(Query, Threshold, BatchSize, Probability),
	reset_state(Program, TransformedRules).

transform(Program, TransformedRules) :-
	% TODO: Cleanup?
	transform_object_program(Program, TransformedRules).

take_samples(Query, Threshold, BatchSize, Probability) :-
	take_samples(Query, Threshold, BatchSize, 0, 0, Probability).
take_samples(Query, Threshold, BatchSize, CurrSamples, CurrSuccesses, Probability) :-
	sample_batch(Query, Successes, BatchSize),
	write(Successes), writeln(' samples succeeded.'),
	NewSamples is CurrSamples + BatchSize,
	NewSuccesses is CurrSuccesses + Successes,
	CurrProbability is NewSuccesses / NewSamples,
	Confidence is 2 * 1.95996 * sqrt(CurrProbability * (1 - CurrProbability) / NewSamples), % see Riguzzi 2013, p. 6
	(Confidence < Threshold ->
		write('In total '), write(NewSuccesses), write('/'), write(NewSamples), writeln(' succeeded.'),
		Probability is CurrProbability
	;
		take_samples(Query, Threshold, BatchSize, NewSamples, NewSuccesses, Probability)
	).

sample_batch(Query, Successes, BatchSize) :-
	write('Sampling batch of size '), writeln(BatchSize),
	sample_batch(Query, 0, Successes, BatchSize).
sample_batch(_Query, CurrSuccesses, Successes, 0) :-
	Successes = CurrSuccesses,
	!.
sample_batch(Query, CurrSuccesses, Successes, Remaining) :-
	(sample_goal(Query) ->
		% write('Sample valid, remaining: '), writeln(Remaining),
		IsValid = 1
		;
		% write('Sample NOT valid, remaining: '), writeln(Remaining),
		IsValid = 0
	),
	NewSuccesses is CurrSuccesses + IsValid,
	NewRemaining is Remaining - 1,
	sample_batch(Query, NewSuccesses, Successes, NewRemaining).


/* 
	Cleanup environment state after running a sampling process to completion.
 */
reset_state(Program, Clauses) :- 
	unload_file(Program), maplist(retract_clause, Clauses).

retract_clause(Head :- _) :- retractall(Head).


	
	
	