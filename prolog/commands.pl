:- module(commands, []).

% TODO: Add commands for:
% - scp
% - ssh run remote script
% - ssh run remote command
% - unzip
% - ps/related things for killing processes
%
% TODO: Move zip command here

java(MainClass, Args, Output) :- java([], MainClass, Args, Output).
java(JavaOpts, MainClass, Args, Output) :-
    builds_with(System, '.'),
    classpath(System, '.', Classpath),
    java(JavaOpts, Classpath, MainClass, Args, Output).

java_cp(Classpath, MainClass, Args, Output) :- java_cp([], Classpath, MainClass, Args, Output).
java_cp(JavaOpts, Classpath, MainClass, Args, Output) :-
    main_classes(Classpath, Classes),
    member(Class, Classes),

    atom_codes(MainClass, MainClassCodes),
    atom_codes(Class, ClassCodes),
    append(_, MainClassCodes, ClassCodes),

    run_java(JavaOpts, Classpath, Class, Args, Output).

run_java(Classpath, MainClass, Args, Output) :- run_java([], Classpath, MainClass, Args, Output).
run_java(JavaOpts, Classpath, MainClass, Args, Output) :-
    append(JavaOpts, ['-cp', Classpath, MainClass], TempArgs),
    append(TempArgs, Args, AllArgs),
    read_process(path(java), AllArgs, Output).

main_classes(Classpath, Classes) :-
    % TODO: Make compatible across different OSes
    atomic_list_concat(Paths, ':', Classpath).

