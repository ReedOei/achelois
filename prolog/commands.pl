:- module(commands, [unzip/2, zip/2,
                     user/2, pid/2, pgid/2, sid/2, ppid/2, proc_c/2, stime/2, tty/2, proc_time/2, cmd/2, process_parent/2, processes/1,
                     find_main_class/2, find_main_class/3,
                     java/3, java/4, java/5,
                     java_cp/4, java_cp/5, java_cp/6,
                     run_java/4, run_java/5, run_java/6]).

:- use_module(library(dcg/basics)).
:- use_module(library(filesex)).

:- use_module(utility).

% TODO: Add commands for:
% - ps/related things for killing processes

lookup_process(Process, N, Val) :-
    functor(Process, process, _),
    arg(N, Process, Val).

user(User, Process) :- lookup_process(Process, 1, User).
pid(PID, Process) :- lookup_process(Process, 2, PID).
ppid(PPID, Process) :- lookup_process(Process, 3, PPID).
pgid(PGID, Process) :- lookup_process(Process, 4, PGID).
sid(SID, Process) :- lookup_process(Process, 5, SID).
proc_c(C, Process) :- lookup_process(Process, 6, C).
stime(STime, Process) :- lookup_process(Process, 7, STime).
tty(TTY, Process) :- lookup_process(Process, 8, TTY).
proc_time(Time, Process) :- lookup_process(Process, 9, Time).
cmd(Cmd, Process) :- lookup_process(Process, 10, Cmd).

process_parent(ProcessA, ProcessB) :-
    functor(ProcessA, process, _),
    functor(ProcessB, process, _),
    ppid(PID, ProcessA),
    pid(PID, ProcessB).

processes(Processes) :-
    read_process(path(ps), ['-ejf'], Output),
    atomic_list_concat(Lines, '\n', Output),
    findall(process(User, PID, PPID, PGID, SID, C, STime, TTY, Time, Cmd),
        (
            member(Line, Lines),
            atomic_list_concat(RawCols, ' ', Line),
            exclude(=(''), RawCols, [User, TempPID, TempPPID, TempPGID, TempSID, TempC, STime, TTY, Time|Cmd]),
            maplist(term_to_atom, [PID, PPID, PGID, SID, C], [TempPID, TempPPID, TempPGID, TempSID, TempC])
        ),
        [_|Processes]). % Exclude the first one, it's the header column

uri_port_arg(Uri, PortFlag, Args) :-
    atomic_list_concat([UriNoPort, Port], ':', Uri) -> Args = [PortFlag, Port, UriNoPort];
    Args = [Uri].

% Note that SSH/SCP commands assume that you have the connection set up
% so you don't need to enter your password manually.
remote_command(Uri, Command, Output) :-
    uri_port_arg(Uri, '-p', Args),
    append(Args, [Command], AllArgs),
    read_process(path(ssh), AllArgs, Output).

% Can either pass in a list of files or just one
% If you don't specify the copy path, it will just be the directory you enter by default when you ssh in
scp(Paths, Uri) :- scp(Paths, Uri, _). % Note that this can take either a single file or a list of files.
scp(Path, Uri, CopyPath) :-
    not(is_list(Path)),
    scp([Path], Uri, CopyPath).
scp(Paths, Uri, CopyPath) :-
    is_list(Paths),
    var(CopyPath),
    remote_command(Uri, 'pwd', Temp),
    atom_concat(CopyPath, '\n', Temp), % pwd prints out a newline, so get rid of that
    scp(Paths, Uri, CopyPath).
scp(Paths, Uri, CopyPath) :-
    is_list(Paths),
    nonvar(CopyPath),

    % If we're passed something like user@host:port
    % then TempUriArgs = ['-P', port, 'user@host']
    % Then we separate out the port arguments from the last argument, which the gets combined with the destination path
    uri_port_arg(Uri, '-P', TempUriArgs),
    last(TempUriArgs, ActualUri),
    append(PortArgs, [ActualUri], TempUriArgs),
    atomic_list_concat([ActualUri, CopyPath], ':', UriArg),

    % Use -r because it doesn't affect regular files but it will automatically fully copy directories
    % which is probably what the user wants to do since they passed in a directory
    flatten([['-r'], PortArgs, Paths, [UriArg]], AllArgs),
    run_process(path(scp), AllArgs).

zip(Dir, ZipName) :-
    file_base_name(Dir, DirName),
    file_name_extension(DirName, 'zip', ZipName),
    run_process(path(zip), ['-r', ZipName, Dir]).

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
java(JavaOpts, MainClass, Args, Output) :- java('.', JavaOpts, MainClass, Args, Output).
java(Path, JavaOpts, MainClass, Args, Output) :-
    quick_classpath(Path, Classpath),
    java_cp(Path, JavaOpts, Classpath, MainClass, Args, Output).

java_cp(Classpath, MainClass, Args, Output) :- java_cp([], Classpath, MainClass, Args, Output).
java_cp(JavaOpts, Classpath, MainClass, Args, Output) :- java_cp('.', JavaOpts, Classpath, MainClass, Args, Output).
java_cp(Path, JavaOpts, Classpath, MainClass, Args, Output) :-
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

    run_java(Path, JavaOpts, Classpath, Class, Args, Output).

run_java(Classpath, MainClass, Args, Output) :- run_java([], Classpath, MainClass, Args, Output).
run_java(JavaOpts, Classpath, MainClass, Args, Output) :- run_java('.', JavaOpts, Classpath, MainClass, Args, Output).
run_java(Path, JavaOpts, Classpath, MainClass, Args, Output) :-
    append(JavaOpts, ['-cp', Classpath, MainClass], TempArgs),
    append(TempArgs, Args, AllArgs),
    read_process(Path, path(java), AllArgs, Output).

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

