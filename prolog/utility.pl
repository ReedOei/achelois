:- module(utility, [string_concat_list/2, intercalate/3, lookup_path/2,
                    read_process/3, read_process/4, read_process/5,
                    read_file/2, write_file/2, list_empty/1, clone_project/3]).

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
    read_process(".", path(which), [ExeName], TempPath),
    replace(TempPath, "\n", "", Path).

read_process(Exe, Args, Output) :- read_process(".", Exe, Args, Output).
read_process(Path, Exe, Args, Output) :- read_process(Path, Exe, Args, Output, _).
read_process(Path, Exe, Args, Output, ExitCode) :-
    catch((
            process_create(Exe, Args, [stdout(pipe(OutputStream)), cwd(Path)]),
            read_string(OutputStream, _, Output),
            close(OutputStream)
          ), error(process_error(_, exit(ExitCode)), _), true).

read_file(Path, Lines) :-
    open(Path, read, Stream),
    read_string(Stream, _, String),
    split_string(String, "\n", "\r", Lines),
    close(Stream).

write_file(OutputPath, Str) :-
    open(OutputPath, write, Stream),
    write(Stream, Str),
    close(Stream).

clone_project(Url, Commit, Path) :-
    file_base_name(Url, TempPath),
    string_concat_list([TempPath, "-", Commit], Path),

    read_process(path(git), ["clone", Url, Path], _),
    read_process(Path, path(git), ["checkout", Commit], _).

list_files(Path, Files) :-
    directory_file_path(Path, "*", Wildcard),
    expand_file_name(Wildcard, Files).

