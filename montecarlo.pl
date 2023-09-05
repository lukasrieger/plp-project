:- module(montecarlo, [montecarlo/4, montecarlo/5, montecarlo/6, montecarlo_no_confidence/5]).

:- use_module(sampler).


/**
 * montecarlo_no_confidence(+File:file, :Query:atom, +SampleCount:int, +SamplingMethod:compound, -Propability:float) is det
 * 
 * The predicate assumes that File designates a PLP object program, which is consulted and then transformed into an equivalent standard prolog program.
 * The predicate samples Query a number of times, as indicated by SampleCount and returns the Probability of the query being true.
 * 
 * The sampling method used by this predicate can be configured via SamplingMethod by either passing the constant `standard` or `(gibbs, BlockSize)`.
 */
montecarlo_no_confidence(File, Query, SampleCount, SamplingMethod, Probability) :-
	resolve_sampling_method(SamplingMethod, SampleVia),
	write('Performing sampling via: '), writeln(SampleVia),
	sampler:load_program(File),
	take_samples_no_confidence(Query, SampleCount, SampleVia, Probability),
	sampler:unload_program.

/**
 * montecarlo(+File:file, :Query:atom, +SamplingMethod:compound, -Propability:float) is det
 * 
 * The predicate assumes that File designates a PLP object program, which is consulted and then transformed into an equivalent standard prolog program.
 * The predicate then samples Query a number of times until the confidence (as detailed in Riguzzi 2013, p. 6) reaches a certain
 * threshold (0.02 by default) and returns the Probability of the query being true.
 * The sampling method used by this predicate can be configured via SamplingMethod by either passing the constant `standard` or `(gibbs, BlockSize)`.
 */
montecarlo(File, Query, SamplingMethod, Probability) :-
	montecarlo(File, Query, 0.02, SamplingMethod, Probability).

/**
 * montecarlo(+File:file, :Query:atom, +Threshold:float, +SamplingMethod:atom, -Propability:float) is det
 * 
 * The predicate assumes that File designates a PLP object program, which is consulted and then transformed into an equivalent standard prolog program.
 * The predicate samples Query a number of times until the confidence (as detailed in Riguzzi 2013, p. 6) reaches the given Threshold.
 * The sampling method used by this predicate can be configured via SamplingMethod by either passing the constant `standard` or `(gibbs, BlockSize)`.
 */
montecarlo(File, Query, Threshold, SamplingMethod, Probability) :-
	montecarlo(File, Query, Threshold, 500, SamplingMethod, Probability).

/**
 * montecarlo(+File:file, :Query:atom, +Threshold:float, +BatchSize: int, +SamplingMethod:atom, -Propability:float) is det
 * 
 * The predicate samples Query in batch sizes of BatchSize until the confidence (as detailed in Riguzzi 2013, p. 6) reaches the given Threshold.
 * The sampling method used by this predicate can be configured via SamplingMethod by either passing the constant `standard` or `(gibbs, BlockSize)`.
 */
montecarlo(File, Query, Threshold, BatchSize, SamplingMethod, Probability) :-
	resolve_sampling_method(SamplingMethod, SampleVia),
	write('Performing sampling via: '), writeln(SampleVia),
	sampler:load_program(File),
	take_samples(Query, Threshold, BatchSize, Probability, SampleVia),
	sampler:unload_program.

take_samples(Query, Threshold, BatchSize, Probability, SampleVia) :-
	take_samples(Query, Threshold, BatchSize, 0, 0, Probability, SampleVia).
take_samples(Query, Threshold, BatchSize, CurrSamples, CurrSuccesses, Probability, SampleVia) :-
	sample_batch(Query, Successes, BatchSize, SampleVia),
	write(Successes), writeln(' samples succeeded.'),
	NewSamples is CurrSamples + BatchSize,
	NewSuccesses is CurrSuccesses + Successes,
	NewProbability is NewSuccesses / NewSamples,
	% See Riguzzi 2013, p. 6:
	Confidence is 2 * 1.95996 * sqrt(NewProbability * (1 - NewProbability) / NewSamples),
	% See p. 9:
	(Confidence < Threshold, (NewSuccesses > 5, NewSamples - NewSuccesses > 5; NewSamples >= 50000) ->
		write('In total '), write(NewSuccesses), write('/'), write(NewSamples), writeln(' succeeded.'),
		Probability is NewProbability
	;
		take_samples(Query, Threshold, BatchSize, NewSamples, NewSuccesses, Probability, SampleVia)
	).

sample_batch(Query, Successes, BatchSize, SampleVia) :-
	write('Sampling batch of size '), writeln(BatchSize),
	sample_batch(Query, 0, Successes, BatchSize, SampleVia).
sample_batch(_Query, CurrSuccesses, Successes, 0, _) :-
	Successes = CurrSuccesses,
	!.
sample_batch(Query, CurrSuccesses, Successes, Remaining, SampleVia) :-
	(call(SampleVia, Query) ->
		IsValid = 1
		;
		IsValid = 0
	),
	NewSuccesses is CurrSuccesses + IsValid,
	NewRemaining is Remaining - 1,
	sample_batch(Query, NewSuccesses, Successes, NewRemaining, SampleVia).



take_samples_no_confidence(Query, SampleCount, SampleVia, Probability) :- 
	take_samples_no_confidence(Query, SampleCount, 0, 0, SampleVia, Probability).

take_samples_no_confidence(Query, SampleCount, CurrSamples, CurrSuccesses, SampleVia, Probability) :-
	sample_round(Query, Success, SampleVia),
	NewSamples is CurrSamples + 1,
	NewSuccesses is CurrSuccesses + Success,
	NewProbability is NewSuccesses / NewSamples,
	(CurrSamples >= SampleCount - 1 ->
		write('In total '), write(NewSuccesses), write('/'), write(NewSamples), writeln(' succeeded.'),
		Probability is NewProbability
		;
		take_samples_no_confidence(Query, SampleCount, NewSamples, NewSuccesses, SampleVia, Probability)
	).

sample_round(Query, Success, SampleVia) :-
	(call(SampleVia, Query) ->
		Success is 1
		;
		Success is 0
	).



resolve_sampling_method(standard, Method) :- Method = sample_goal.

resolve_sampling_method((gibbs, BlockSize), Method) :- Method =.. [sample_goal_gibbs, BlockSize].