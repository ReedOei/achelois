:- module(try_compile, [configurations/3, compiles/1, compiles/2, compiles/3]).

:- use_module(library(filesex)).

:- use_module(build_systems).
:- use_module(utility).

configurations(RepoPath, System, Goal) :-
    build_system(System),
    goal(Goal),
    builds_with(System, RepoPath).

% This will only be true if the build succeeds
compiles(Path) :- compiles(Path, compile).
compiles(Path, Goal) :- builds_with(System, Path), compiles(Path, System, Goal).
compiles(Path, System, Goal) :-
    (
        run_compile(Path, System, Goal, Output);

        absolute_file_name(Path, Absolute),
        file_directory_name(Absolute, ParentPath),
        run_compile(ParentPath, System, Goal, Output)
    ),
    success_string(System, SuccessString),
    sub_string(Output, _, _, _, SuccessString).

run_compile(Path, System, Goal, Output) :-
    % Retrieve information about the build system so we can actually run it.
    exe_name(System, SystemPath),
    goal_name(System, Goal, GoalName),

    read_process(Path, SystemPath, [GoalName], Output),

    directory_file_path(Path, "compile-output.txt", OutputPath),
    write_file(OutputPath, Output).

