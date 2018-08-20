:- module(commands, [unzip/2, zip/2,
                     user/2, pid/2, pgid/2, sid/2, ppid/2, proc_c/2, stime/2, tty/2, proc_time/2, cmd/2, process_parent/2, processes/1,
                     remote_pwd/2, remote_absolute_path/3,
                     find_main_class/2, find_main_class/3,
                     java/2, java/3, java_cp/4, run_java/4]).

:- use_module(library(dcg/basics)).
:- use_module(library(filesex)).

:- use_module(utility).

lookup_process(Process, N, Val) :-
    var(Process),
    processes(Processes),
    member(Process, Processes),
    lookup_process(Process, N, Val).
lookup_process(Process, N, Val) :-
    nonvar(Process),
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
    nonvar(ProcessA), nonvar(ProcessB),
    ppid(PID, ProcessA),
    pid(PID, ProcessB).
process_parent(ProcessA, ProcessB) :-
    var(ProcessA), nonvar(ProcessB),
    pid(PID, ProcessB),
    processes(Processes),
    member(ProcessA, Processes),
    ppid(PID, ProcessA).
process_parent(ProcessA, ProcessB) :-
    nonvar(ProcessA), var(ProcessB),
    ppid(PID, ProcessA),
    processes(Processes),
    member(ProcessB, Processes),
    pid(PID, ProcessB).
process_parent(ProcessA, ProcessB) :-
    var(ProcessA), var(ProcessB),
    processes(Processes),
    member(ProcessA, Processes),
    member(ProcessB, Processes),
    process_parent(ProcessA, ProcessB).

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

remote_pwd(Uri, Pwd) :-
    remote_command(Uri, 'pwd', Temp),
    atom_concat(Pwd, '\n', Temp). % pwd prints out a newline, so get rid of that

remote_absolute_path(Uri, RelPath, AbsPath) :-
    var(RelPath),
    nonvar(AbsPath),
    remote_pwd(Uri, Pwd),
    directory_file_path(Pwd, RelPath, AbsPath).

remote_absolute_path(Uri, RelPath, AbsPath) :-
    nonvar(RelPath),

    (
        atom_concat('/', _, RelPath) -> AbsPath = RelPath;

        % If it's not an absolute path, make it absolute by assuming it's relative to the pwd when you ssh in
        remote_pwd(Uri, Pwd),
        directory_file_path(Pwd, RelPath, AbsPath)
    ).

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
scp(Paths, Uri, InPath) :-
    is_list(Paths),
    nonvar(InPath),

    remote_absolute_path(Uri, InPath, CopyPath),

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

java(MainClass, Args) :- java(MainClass, Args, []).
java(MainClass, Args, Options) :-
    process_options(Options, Path, _),
    quick_classpath(Path, Classpath),
    java_cp(Classpath, MainClass, Args, Options).

java_cp(Classpath, MainClass, Args, Options) :-
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

    run_java(Classpath, Class, Args, Options).

run_java(Classpath, MainClass, InArgs, Options) :-
    make_args(InArgs, Args),

    process_options(Options, Path, JavaOpts),

    append(JavaOpts, ['-cp', Classpath, MainClass], TempArgs),
    append(TempArgs, Args, AllArgs),

    (
        % TODO: Figure out how to pass java options when using mvn exec plugin
        builds_with(maven, Path), JavaOpts = [] -> mvn_exec(MainClass, Args, Options);

        process(path(java), AllArgs, Options)
    ).

process_options(Options, Path, JavaOpts) :-
    (
        member(path(Path), Options);
        Path = '.'
    ),

    findall(JavaOpt, member(java_opt(JavaOpt), Options), JavaOpts).

make_args(Args, ActualArgs) :-
    maplist(make_arg, Args, Temp),
    flatten(Temp, ActualArgs).
make_arg(ArgName=ArgValue, [Result]) :-
    % If the arg name already starts with a '-', don't change it
    not(atom_concat('-', ArgName)) ->
        (
            atom_chars(ArgName, [_]) ->
                atomic_list_concat(['-', ArgName, '=', ArgValue], '', Result);
            atomic_list_concat(['--', ArgName, '=', ArgValue], '', Result)
        );
    atomic_list_concat([ArgName, '=', ArgValue], Result).
make_arg(ArgName-ArgValue, [ActualArgName, ArgValue]) :-
    atom_chars(ArgName, [_]) -> atom_concat('-', ArgName, ActualArgName);

    atom_concat('--', ArgName, ActualArgName).

mvn_exec(MainClass, Args, Options) :-
    atom_concat('-Dexec.mainClass=', MainClass, MainClassArg),
    maplist(surround_atom('\'', '\''), Args, QuotedArgs),
    atomic_list_concat(QuotedArgs, ' ', ArgStr),
    atom_concat('-Dexec.args=', ArgStr, MavenArgs),

    process(path(mvn), ['package', '-DskipTests', 'exec:java', MainClassArg, MavenArgs], Options).

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

