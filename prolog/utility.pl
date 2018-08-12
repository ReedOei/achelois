:- module(utility, [string_concat_list/2, intercalate/3, lookup_path/2,
                    read_process/3, read_process/4, read_process/5,
                    read_file/2, write_file/2, list_empty/1,
                    list_files/2, run_process/2, run_process/3, run_process/4,
				    walk/2, take_while/3, take/3, cache/3, cache_global/3,
                    invalidate_cache/0, invalidate_cache/1]).

:- use_module(library(filesex)).
:- use_module(library(clpfd)).

factorial(0, 1).
factorial(N, F) :-
    N1 #= N - 1,
    factorial(N1, F1),
    F #= N * F1.

% Delete everything
invalidate_cache :-
    cache_path('Test', AcheloisPath, _),
    exists_directory(AcheloisPath),
    delete_directory_and_contents(AcheloisPath).

% Delete just one part of the cache
invalidate_cache(BasePath) :-
    cache_path(BasePath, AcheloisPath, Path),
    (
        exists_file(Path),
        delete_file(Path);

        exists_directory(Path),
        delete_directory_and_contents(Path)
    ),

    % Check if cache is empty and delete everything if so
    (
        exists_directory(AcheloisPath),
        list_files(AcheloisPath, []),
        delete_directory_and_contents(AcheloisPath);

        true
    ).

cache_path(BasePath, AcheloisPath, Path) :-
    working_directory(CWD, CWD),
    directory_file_path(CWD, '.achelois', AcheloisPath),
    directory_file_path(AcheloisPath, BasePath, Path).

cache(BasePath, Pred, Data) :-
    cache_path(BasePath, _, Path),
    cache_global(Path, Pred, Data).

cache_global(BasePath, Pred, Data) :-
    read_cache(BasePath, Data);

    call(Pred),
    write_cache(BasePath, Data).

read_cache(Path, Data) :-
    exists_file(Path),
    read_file(Path, [Atom]),
    term_to_atom(Data, Atom).

write_cache(Path, Data) :-
    file_directory_name(Path, Dir),
    make_directory_path(Dir),
    term_to_atom(Data, Atom),
    write_file(Path, Atom).

list_empty([]).

replace(Str, Search, Rep, OutStr) :-
    split_string(Str, Search, "", StrList),
    join(StrList, Rep, OutStr).

join(List, Sep, Str) :-
    intercalate(List, Sep, StrList),
    string_concat_list(StrList, Str).

string_concat_list([], "").
string_concat_list([H|T], Str) :-
    string_concat_list(T, TempStr),
    string_concat(H, TempStr, Str).

intercalate([], _, []).
intercalate([H], _, [H]).
intercalate([H|T], Sep, [H, Sep | List]) :- intercalate(T, Sep, List).

lookup_path(ExeName, Path) :-
    read_process('.', path(which), [ExeName], TempPath),
    atomic_list_concat([Path|_], '\n', TempPath).

run_process(Exe, Args) :- run_process('.', Exe, Args).
run_process(Path, Exe, Args) :- run_process(Path, Exe, Args, _).
run_process(Path, Exe, Args, ExitCode) :-
    process_create(Exe, Args, [cwd(Path), process(PID), detached(true)]),
    process_wait(PID, exit(ExitCode)).

read_process(Exe, Args, Output) :- read_process('.', Exe, Args, Output).
read_process(Path, Exe, Args, Output) :- read_process(Path, Exe, Args, Output, _).
read_process(Path, Exe, Args, Output, ExitCode) :-
    process_create(Exe, Args, [stdout(pipe(OutputStream)), stderr(pipe(OutputStream)), cwd(Path), process(PID), detached(true)]),
    read_string(OutputStream, _, OutputStr),
    atom_string(Output, OutputStr),
    process_wait(PID, exit(ExitCode)),
    close(OutputStream).

read_file(Path, Lines) :-
    open(Path, read, Stream),
    read_string(Stream, _, String),
    atom_string(Atom, String),
    atomic_list_concat(Lines, '\n', Atom),
    close(Stream).

write_file(OutputPath, Str) :-
    open(OutputPath, write, Stream),
    write(Stream, Str),
    close(Stream).

list_files(Path, Files) :-
    directory_file_path(Path, '*', Wildcard),
    expand_file_name(Wildcard, Files).

walk(Path, Result) :-
    exists_directory(Path),
    list_files(Path, TempFiles),

    member(File, TempFiles),
    (
        Result = File;

        exists_directory(File),
        walk(File, Result)
    ).
walk(Path, Path) :- exists_file(Path).

take_while(_, [], []).
take_while(Pred, [H|T], [H|Rest]) :- call(Pred, H), take_while(Pred, T, Rest).
take_while(_, _, []).

take(_, [], []).
take(0, _, []).
take(N, [H|T], [H|Rest]) :- N1 is N - 1, take(N1, T, Rest).

same_length(A, B, NewA, NewB) :-
    length(A, LenA),
    length(B, LenB),
    take(LenB, A, NewA),
    take(LenA, B, NewB).

sublist([], _).
sublist(SubList, List) :-
    var(SubList),
    SubList = [H|T],
    member(H, List),
    select(H, List, NewList),
    sublist(T, NewList).
sublist(SubList, List) :-
    nonvar(SubList),
    forall(member(X, SubList), member(X, List)).

