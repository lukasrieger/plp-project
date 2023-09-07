:- module(montecarlo, [montecarlo/3, montecarlo/4]).

:- use_module(sampler).

/**
 * montecarlo(+File:file, :Query:atom, -Probability:float, +Options:list) is det
 *
 * The predicate assumes that `File` designates a PLP object program, which is consulted and
 * transformed into an equivalent standard prolog program. The predicate then samples `Query`
 * either a fixed number of times or until a confidence value (as detailed in Riguzzi 2013, p. 6)
 * reaches a configured threshold and returns the sampled `Probability` of the query being true.
 * Non-ground queries are treated as existential queries.
 *
 * `Options` is a list configuring the sampling process. Available options:
 * * sampler(+SamplerParams:list)
 *   First parameter is either `standard` or `gibbs`. When using Gibbs sampling, a second
 *   parameter controls the block size of variables sampled together.
 * * confidence(+Confidence:float)
 *   Configures the amount of samples taken by defining an expected target accuracy for the result.
 *   Set to a value other than a number to instead take a fixed amount of samples.
 * * count(+Count:int)
 *   Amount of samples taken.
 *   (confidence unset -> in total; confidence set -> at once between calulating confidence values)
 * * silent(+Silent:int)
 *   Suppress logging informational output (if value > 0).
 */
montecarlo(File, Query, Probability, Options) :-
	Defaults = [
		sampler(standard),
		count(1000),
		confidence(0.02),
		silent(0)
	],
	merge_options(Options, Defaults, Opts),
	option(sampler(SamplerParams), Opts),
	option(count(Count), Opts),
	option(confidence(Confidence), Opts),
	option(silent(Silent), Opts),
	resolve_sampler(SamplerParams, SamplerOpts),
	(Silent > 0 -> !; writef('Using sampler: %w\n', [SamplerOpts])),
	sampler:load_program(File),
	(number(Confidence) ->
		(Silent > 0 -> !; writef('Taking batches of %w samples until confidence < %w.\n', [Count, Confidence])),
		take_samples_confidence(Query, Confidence, Count, SamplerOpts, Probability, Samples, Successes)
		;
		(Silent > 0 -> !; writef('Taking %w samples.\n', [Count])),
		take_samples_fixed(Query, Count, SamplerOpts, Probability, Samples, Successes)
	),
	(Silent > 0 -> !; writef('%w/%w samples succeeded.\n', [Successes, Samples])),
	sampler:unload_program.

/**
 * montecarlo(+File:file, :Query:atom, -Probability:float) is det
 *
 * Alias of montecarlo/3 with empty options.
 */
montecarlo(File, Query, Probability) :-
	montecarlo(File, Query, Probability, []).

resolve_sampler(Params, Sampler) :-
	(is_list(Params) -> fail; resolve_sampler([Params], Sampler)).
resolve_sampler([standard], Sampler) :- Sampler = sample_goal, !.
resolve_sampler([gibbs], Sampler) :- Sampler =.. [sample_goal_gibbs, 1], !.
resolve_sampler([gibbs, BlockSize], Sampler) :- Sampler =.. [sample_goal_gibbs, BlockSize], !.

take_samples_confidence(Query, Threshold, BatchSize, SamplerOpts, Probability, Samples, Successes) :-
	take_samples_confidence(Query, Threshold, BatchSize, 0, 0, SamplerOpts, Probability, Samples, Successes).
take_samples_confidence(Query, Threshold, BatchSize, CurrSamples, CurrSuccesses, SamplerOpts, Probability, Samples, Successes) :-
	% write('Sampling batch of size '), writeln(BatchSize),
	sample_batch(Query, BatchSuccesses, BatchSize, SamplerOpts),
	% write(Successes), writeln(' samples succeeded.'),
	NewSamples is CurrSamples + BatchSize,
	NewSuccesses is CurrSuccesses + BatchSuccesses,
	NewProbability is NewSuccesses / NewSamples,
	% See Riguzzi 2013, p. 6:
	Confidence is 2 * 1.95996 * sqrt(NewProbability * (1 - NewProbability) / NewSamples),
	% See p. 9:
	(Confidence < Threshold, (NewSuccesses > 5, NewSamples - NewSuccesses > 5; NewSamples >= 50000) ->
		Samples is NewSamples,
		Successes is NewSuccesses,
		Probability is NewProbability
	;
		take_samples_confidence(Query, Threshold, BatchSize, NewSamples, NewSuccesses, SamplerOpts, Probability, Samples, Successes)
	).

sample_batch(Query, Successes, BatchSize, SamplerOpts) :-
	sample_batch(Query, 0, Successes, BatchSize, SamplerOpts).
sample_batch(_Query, CurrSuccesses, Successes, 0, _) :-
	Successes = CurrSuccesses,
	!.
sample_batch(Query, CurrSuccesses, Successes, Remaining, SamplerOpts) :-
	(call(SamplerOpts, Query) ->
		IsValid = 1
		;
		IsValid = 0
	),
	NewSuccesses is CurrSuccesses + IsValid,
	NewRemaining is Remaining - 1,
	sample_batch(Query, NewSuccesses, Successes, NewRemaining, SamplerOpts).

take_samples_fixed(Query, SampleCount, SamplerOpts, Probability, Samples, Successes) :-
	take_samples_fixed(Query, SampleCount, 0, 0, SamplerOpts, Probability, Samples, Successes).
take_samples_fixed(Query, SampleCount, CurrSamples, CurrSuccesses, SamplerOpts, Probability, Samples, Successes) :-
	sample_round(Query, Success, SamplerOpts),
	NewSamples is CurrSamples + 1,
	NewSuccesses is CurrSuccesses + Success,
	NewProbability is NewSuccesses / NewSamples,
	(NewSamples == SampleCount ->
		Samples is NewSamples,
		Successes is NewSuccesses,
		Probability is NewProbability
	;
		take_samples_fixed(Query, SampleCount, NewSamples, NewSuccesses, SamplerOpts, Probability, Samples, Successes)
	).

sample_round(Query, Success, SamplerOpts) :-
	(call(SamplerOpts, Query) ->
		Success is 1
		;
		Success is 0
	).