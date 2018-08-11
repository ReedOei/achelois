:- module(commands, [unzip/2, find_main_class/2, find_main_class/3]).

:- use_module(library(dcg/basics)).
:- use_module(library(filesex)).

:- use_module(utility).

% TODO: Add commands for:
% - scp
% - ssh run remote script
% - ssh run remote command
% - unzip
% - ps/related things for killing processes
%
% TODO: Move zip command here

unzip(ZipFile, Destination) :-
    var(Destination),
    file_name_extension(Destination, 'zip', ZipFile),
    read_process(path(unzip), [ZipFile, '-d', Destination], _),

    (
        % Check to make sure we don't do something like extra a.zip into ./a/a
        file_base_name(Destination, Base),
        directory_file_path(Destination, Base, Inside),
        exists_directory(Inside),

        % Instead convert the file structure to be just ./a
        copy_directory(Inside, Destination),
        delete_directory_and_contents(Inside);

        true
    ).

unzip(ZipFile, Destination) :-
    nonvar(Destination),
    read_process(path(unzip), [ZipFile, '-d', Destination], _).

java(MainClass, Args, Output) :- java([], MainClass, Args, Output).
java(JavaOpts, MainClass, Args, Output) :-
    quick_classpath('.', Classpath),
    java_cp(JavaOpts, Classpath, MainClass, Args, Output).

java_cp(Classpath, MainClass, Args, Output) :- java_cp([], Classpath, MainClass, Args, Output).
java_cp(JavaOpts, Classpath, MainClass, Args, Output) :-
    cache(MainClass,
        (
            find_main_class(Classpath, MainClass, Class),

            atom_codes(MainClass, MainClassCodes),
            atom_codes(Class, ClassCodes),
            append(_, MainClassCodes, ClassCodes)
        ), Class), !,

    % Clear out the temp jar files. We only create a temp file so we can get the temp directory
    tmp_file('basic', TmpFile),
    file_directory_name(TmpFile, Dir),
    foreach((walk(Dir, File), sub_atom(File, _, _, _, 'expandedjarfile')), delete_directory_and_contents(File)), !,

    run_java(JavaOpts, Classpath, Class, Args, Output).

run_java(Classpath, MainClass, Args, Output) :- run_java([], Classpath, MainClass, Args, Output).
run_java(JavaOpts, Classpath, MainClass, Args, Output) :-
    append(JavaOpts, ['-cp', Classpath, MainClass], TempArgs),
    append(TempArgs, Args, AllArgs),
    read_process(path(java), AllArgs, Output).

find_main_class(Classpath, MainClass) :- find_main_class(Classpath, '', MainClass).
find_main_class(Classpath, SubStr, MainClass) :-
    % TODO: Make compatible across different OSes
    atomic_list_concat(Paths, ':', Classpath),
    member(Path, Paths),
    read_main_class(Path, SubStr, MainClass).

read_main_class(Path, SubStr, MainClass) :-
    file_name_extension(_, 'jar', Path),
    read_main_class_jar(Path, SubStr, MainClass);

    exists_directory(Path),
    read_main_class_dir(Path, SubStr, MainClass).

read_main_class_jar(Path, SubStr, MainClass) :-
    tmp_file('expandedjarfile', Dir),
    make_directory(Dir),

    unzip(Path, Dir),
    read_main_class_dir(Dir, SubStr, MainClass).

read_main_class_dir(Path, SubStr, MainClass) :-
    walk(Path, File),
    file_name_extension(_, 'class', File),
    main_class(File, SubStr, MainClass).

main_class(File, SubStr, Class) :-
    sub_atom(File, _, _, _, SubStr),
    read_process(path(javap), [File], Output), !,
    class_name(Output, Class), !,
    sub_atom(Output, _, _, _, 'public static void main').

class_name(Output, ClassName) :-
    atom_codes(Output, Codes),
    phrase(java_class(ClassNameCodes), Codes),
    atom_codes(ClassName, ClassNameCodes).

java_class(ClassName) -->
    string(_),
    "public class ", string_without("\n{ ", ClassName),
    (" "; "{"), string(_).

