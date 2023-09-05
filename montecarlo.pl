:- module(montecarlo, [montecarlo/3, montecarlo/4, montecarlo/5]).

:- use_module(sampler).

montecarlo(File, Query, Probability) :-
	montecarlo(File, Query, 0.02, Probability).
montecarlo(File, Query, Threshold, Probability) :-
	montecarlo(File, Query, Threshold, 500, Probability).
montecarlo(File, Query, Threshold, BatchSize, Probability) :-
	sampler:load_program(File),
	take_samples(Query, Threshold, BatchSize, Probability),
	sampler:unload_program.

take_samples(Query, Threshold, BatchSize, Probability) :-
	take_samples(Query, Threshold, BatchSize, 0, 0, Probability).
take_samples(Query, Threshold, BatchSize, CurrSamples, CurrSuccesses, Probability) :-
	sample_batch(Query, Successes, BatchSize),
	% write(Successes), writeln(' samples succeeded.'),
	NewSamples is CurrSamples + BatchSize,
	NewSuccesses is CurrSuccesses + Successes,
	NewProbability is NewSuccesses / NewSamples,
	% See Riguzzi 2013, p. 6:
	Confidence is 2 * 1.95996 * sqrt(NewProbability * (1 - NewProbability) / NewSamples),
	% See p. 9:
	(Confidence < Threshold, (NewSuccesses > 5, NewSamples - NewSuccesses > 5; NewSamples >= 50000) ->
		write(NewSuccesses), write('/'), write(NewSamples), writeln(' samples succeeded.'),
		Probability is NewProbability
	;
		take_samples(Query, Threshold, BatchSize, NewSamples, NewSuccesses, Probability)
	).

sample_batch(Query, Successes, BatchSize) :-
	% write('Sampling batch of size '), writeln(BatchSize),
	sample_batch(Query, 0, Successes, BatchSize).
sample_batch(_Query, CurrSuccesses, Successes, 0) :-
	Successes = CurrSuccesses,
	!.
sample_batch(Query, CurrSuccesses, Successes, Remaining) :-
	(sampler:sample_goal(Query) ->
		IsValid = 1
		;
		IsValid = 0
	),
	NewSuccesses is CurrSuccesses + IsValid,
	NewRemaining is Remaining - 1,
	sample_batch(Query, NewSuccesses, Successes, NewRemaining).
