:- module(try_compile, [try_compile/2]).

:- use_module(library(filesex)).

:- use_module(build_systems).
:- use_module(utility).

try_compile(Url, Successful) :-
    project_name(Url, Name),
    run_setup(Url, Name, ResultPath, Commits),
    repo_path(Name, RepoPath),
    findall([Commit, System, Goal],
            (
                member(Commit, Commits),
                \+ list_empty(Commit),
                configurations(RepoPath, System, Goal),
                compiles(Name, Commit, System, Goal)
            ),
            Successful),
    directory_file_path(ResultPath, "successful.txt", SuccessfulPath),
    write_file(SuccessfulPath, Successful).

configurations(RepoPath, System, Goal) :-
    build_system(System),
    goal(Goal),
    builds_with(System, RepoPath).

% This will only be true if the build succeeds
compiles(Name, Commit, System, Goal) :-
    string_concat_list(["Using ", System, " ", Goal, " to try commit: ", Commit], Msg),
    writeln(Msg),
    run_compile(Name, Commit, System, Goal, Output),
    success_string(System, SuccessString),
    sub_string(Output, _, _, _, SuccessString).

run_compile(Name, Commit, System, Goal, Output) :-
    results_path(Name, ResultPath),
    repo_path(Name, RepoPath),

    tmp_file("temp-repo", TempRepoPath),
    copy_directory(RepoPath, TempRepoPath),

    % Retrieve information about the build system so we can actually run it.
    exe_name(System, SystemPath),
    goal_name(System, Goal, GoalName),
    read_process(TempRepoPath, SystemPath, [GoalName], Output),

    string_concat_list([Commit, "-", GoalName, "-output.txt"] , OutputFilename),
    directory_file_path(ResultPath, OutputFilename, OutputPath),
    write_file(OutputPath, Output).

run_setup(Url, Name, ResultPath, Commits) :-
    setup_dirs(Name, ResultPath),
    clone_project(Url, Name, ResultPath),
    write_commits(Name, ResultPath, Commits).

write_commits(Name, ResultPath, Commits) :-
    repo_path(Name, RepoPath),
    read_process(RepoPath, path(git), ["log", "-s", "--format=%H"], Output),
    directory_file_path(ResultPath, "commits.txt", OutputPath),
    write_file(OutputPath, Output),
    split_string(Output, "\n", "", Commits).

% The path of the directory that contains the main repository for this project.
repo_path(Name, RepoPath) :-
    string_concat(Name, "-temp", TempPath),
    results_path(Name, Path),
    directory_file_path(Path, TempPath, RepoPath).

% The path of the directory that contains all of the results for this project.
results_path(Name, Path) :-
    string_concat(Name, "-results", DirName),
    directory_file_path(".", DirName, TempPath),
    absolute_file_name(TempPath, Path).

setup_dirs(Name, Path) :-
    results_path(Name, Path),
    make_directory_path(Path).

clone_project(Url, Name, ResultPath) :-
    repo_path(Name, RepoPath),
    read_process(ResultPath, path(git), ["clone", Url, RepoPath], _).

