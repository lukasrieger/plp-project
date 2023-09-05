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
	montecarlo(File, Query, ResultP),
	writef('Expected: %w\n  Result: %w\n\n', [ExpectedP, ResultP]).
