:- module(path, [file_stream/4, build_path/2, parent_path/2, make_parent/1,
                 file_format/2, nth_parent_dir/3, walk/2, walk/3, list_files/2]).

:- use_module(library(clpfd)).
:- use_module(library(filesex)).

file_stream(File, Mode, Stream, Pred) :-
    setup_call_cleanup(
        open(File, Mode, Stream),
        call(Pred),
        close(Stream)).

build_path(Paths, Path) :-
    reverse(Paths, RevPaths), % Because of how foldl/directory_file_path works
    foldl(directory_file_path, RevPaths, '', Path).

parent_path(Path, Parent) :-
    atom_concat(NoTrailingSlash, '/', Path) -> directory_file_path(Parent, _, NoTrailingSlash);
    directory_file_path(Parent, _, Path).

make_parent(Path) :-
    parent_path(Path, Parent),
    make_directory_path(Parent).

file_format(Path, Format) :-
    file_name_extension(_, Ext, Path),
    downcase_atom(Ext, Format).

nth_parent_dir(0, Path, Path).
nth_parent_dir(N, Path, Parent) :-
    N #> 0,
    absolute_file_name(Path, AbsPath),
    file_directory_name(AbsPath, TempParent),
    N1 #= N - 1,
    nth_parent_dir(N1, TempParent, Parent).

walk(Path, Result) :- walk(Path, always, Result).

walk(Path, Pred, Result) :-
    call(Pred, Path),
    exists_directory(Path),
    directory_files(Path, Files),
    member(Temp, Files),
    not(Temp = '.'),
    not(Temp = '..'),
    directory_file_path(Path, Temp, File),
    call(Pred, File),
    (
        Result = File;

        exists_directory(File),
        walk(File, Pred, Result)
    ).
walk(Path, Pred, Path) :- call(Pred, Path), exists_file(Path).

list_files(Path, Files) :-
    directory_file_path(Path, '*', Wildcard),
    expand_file_name(Wildcard, Files).

