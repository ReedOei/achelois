:- module(try_compile, [configurations/3, compiles/4]).

:- use_module(library(filesex)).

:- use_module(build_systems).
:- use_module(utility).

configurations(RepoPath, System, Goal) :-
    build_system(System),
    goal(Goal),
    builds_with(System, RepoPath).

% This will only be true if the build succeeds
compiles(Path, Commit, System, Goal) :-
    string_concat_list(["Using ", System, " ", Goal, " on ", Path, " to try commit: ", Commit], Msg),
    writeln(Msg),
    run_compile(Path, Commit, System, Goal, Output),
    success_string(System, SuccessString),
    sub_string(Output, _, _, _, SuccessString).

run_compile(Path, Commit, System, Goal, Output) :-
    % Retrieve information about the build system so we can actually run it.
    exe_name(System, SystemPath),
    goal_name(System, Goal, GoalName),
    read_process(TempRepoPath, SystemPath, [GoalName], Output),

    string_concat_list([Commit, "-", GoalName, "-output.txt"] , OutputFilename),
    directory_file_path(ResultPath, OutputFilename, OutputPath),
    write_file(OutputPath, Output).

