:- module(try_compile, [configurations/3, compiles/1, compiles/2, compiles/3, compiles/4]).

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
compiles(Path, System, Goal) :- compiles(Path, System, Goal, []).
compiles(Path, System, Goal, _) :- files_exist(Path, System, Goal).
compiles(Path, System, Goal, Args) :-
    not(files_exist(Path, System, Goal)),

    (
        run_compile(Path, System, Goal, Args, Output);

        absolute_file_name(Path, Absolute),
        file_directory_name(Absolute, ParentPath),
        run_compile(ParentPath, System, Goal, Args, Output)
    ),
    success_string(System, SuccessString),
    sub_atom(Output, _, _, _, SuccessString).

files_exist(Path, System, Goal) :-
    goal_files(System, Goal, Files),
    forall(member(File, Files),
        (
            directory_file_path(Path, File, FullPath),
            ( exists_file(FullPath); exists_directory(FullPath) )
        )).

run_compile(Path, System, Goal, CustomArgs, Output) :-
    % Retrieve information about the build system so we can actually run it.
    exe_name(System, SystemPath),
    goal_args(System, Goal, GoalArgs),

    output_file(Path, System, Goal, CustomArgs, OutputPath),

    append(GoalArgs, CustomArgs, AllArgs),
    process(SystemPath, AllArgs, [path(Path), output(Output)]),
    write_file(OutputPath, Output).

output_file(Path, System, Goal, CustomArgs, OutputPath) :-
    maplist(surround_atom('\'', '\''), CustomArgs, QuotedArgs),
    atomic_list_concat(QuotedArgs, '-', ArgsPart),
    atomic_list_concat(['output', System, Goal, ArgsPart], '-', TempFilename),
    file_name_extension(TempFilename, 'txt', Filename),
    directory_file_path(Path, Filename, OutputPath).

