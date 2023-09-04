:- module(montecarlo, [montecarlo/4, montecarlo/5, montecarlo/6]).

:- use_module(sampler).


montecarlo(File, Query, Probability, SamplingMethod) :-
	montecarlo(File, Query, 0.02, Probability, SamplingMethod).
montecarlo(File, Query, Threshold, Probability, SamplingMethod) :-
	montecarlo(File, Query, Threshold, 500, Probability, SamplingMethod).
montecarlo(File, Query, Threshold, BatchSize, Probability, SamplingMethod) :-
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




resolve_sampling_method(standard, Method) :- Method = sampler:sample_goal.

resolve_sampling_method(gibbs, Method) :- Method = sampler:sample_goal_gibbs.