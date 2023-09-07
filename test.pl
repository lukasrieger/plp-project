:- module(test, [
	mc_tests/0, mc_tests/1,
	mc_test/3
]).

:- use_module(montecarlo).

mc_tests :-
	mc_tests('./test').
mc_tests(Directory) :-
	directory_files(Directory, DirFilesRelative),
	add_directory(DirFilesRelative, Directory, DirFiles),
	include(exists_file, DirFiles, Files),
	maplist(consult, Files).
add_directory([], _Directory, []) :-
	!.
add_directory([File | RestFiles], Directory, [OutputFile | RestOutputFiles]) :-
	string_concat(Directory, "/", DirWithSlash),
	string_concat(DirWithSlash, File, OutputFile),
	add_directory(RestFiles, Directory, RestOutputFiles).


mc_test(File, Query, ExpectedP) :-
	writef('Testing file "%w" for query "%w"...\n', [File, Query]),
	format('Expected: ~w\n', [ExpectedP]),
	mc_test_single(File, Query, [sampler(standard)]),
	mc_test_single(File, Query, [sampler(gibbs)]),
	writeln('').
mc_test_single(File, Query, Options) :-
	merge_options(Options, [silent(1)], Opts),
	montecarlo(File, Query, ResultP, Opts),
	format('  Result: ~4f ~w\n', [ResultP, Options]).
