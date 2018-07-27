:- module(utility, [string_concat_list/2, intercalate/3, lookup_path/2,
                    read_process/3, read_process/4, read_process/5,
                    read_file/2, write_file/2, list_empty/1,
                    list_files/2, run_process/2, run_process/3, run_process/4,
				    walk/2, zip/2]).

:- use_module(library(filesex)).

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

zip(Dir, ZipName) :-
    file_base_name(Dir, DirName),
    file_name_extension(DirName, 'zip', ZipName),
    run_process(path(zip), ['-r', ZipName, Dir]).

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

walk(Path, Files) :-
    exists_directory(Path),
    list_files(Path, TempFiles),
    findall(ChildFiles, (member(Dir, TempFiles), exists_directory(Dir), walk(Dir, ChildFiles)), ChildFileList),
    flatten([TempFiles|ChildFileList], Files).
walk(Path, [Path]) :- exists_file(Path).
walk(_, []). % Path isn't a file or directory, so it just doesn't exist.

