:- module(utility, [string_concat_list/2, intercalate/3, lookup_path/2,
                    process/2, process/3,
                    read_file/2, read_file_lines/2, write_file/2, list_empty/1,
                    run_process/2, run_process/3, run_process/4,
                    cache/3, cache_global/3,
                    drop/3, drop_while/3, split/4, take_while/3, take/3,
                    always/1,
                    startswith/2, endswith/2,
                    delete_cache/0, delete_cache/1,
                    chars_of_type/2, numbers/1, letters_lower/1, letters_upper/1, letters/1,
                    unique/1, unique/2,
                    sublist/2,
                    surround_atom/4,
                    parse/2,
                    call_if_var/2,
                    line/2, next_matching/2,
                    range/3,
                    partition/2, selectN/4, group/3,
                    call_or_inverse/2, apply_each/2,
                    pad_begin/4, pad_end/4, repeated/2]).

:- use_module(library(filesex)).
:- use_module(library(clpfd)).

:- use_module(term_util).
:- use_module(path).

startswith(A, B) :- atom_concat(B, _, A).
endswith(A, B) :- atom_concat(_, B, A).

repeated(_, []).
repeated(X, [X|Xs]) :- repeated(X, Xs).

pad_begin(N, E, Xs, NewXs) :-
    length(Xs, L), L #=< N ->
        Dif #= N - L,
        length(Padding, Dif),
        repeated(E, Padding),
        append(Padding, Xs, NewXs);
    Xs = NewXs.

pad_end(N, E, Xs, NewXs) :-
    length(Xs, L), L #=< N ->
        Dif #= N - L,
        length(Padding, Dif),
        repeated(E, Padding),
        append(Xs, Padding, NewXs);
    Xs = NewXs.

selectN(0, [], List, List).
selectN(N, [X|Xs], List, NewList) :-
    N #> 0,
    select(X, List, Temp),

    N1 #= N - 1,
    selectN(N1, Xs, Temp, NewList).

partition([], []).
partition(List, [Partition|Partitions]) :-
    length(List, L),
    between(1, L, N),
    selectN(N, Partition, List, NewList),
    partition(NewList, Partitions).

group(_, [], []).
group(N, List, []) :-
    length(List, L),
    L #< N.
group(N, List, [Group|Groups]) :-
    length(Group, N),
    append(Group, NewList, List),
    group(N, NewList, Groups).

drop(0, Xs, Xs).
drop(N, [_|Xs], Rest) :-
    N #> 0,
    N1 #= N - 1,
    drop(N1, Xs, Rest).

drop_while(_, [], []).
drop_while(Pred, [X|Xs], Rest) :-
    call(Pred, X) -> drop_while(Pred, Xs, Rest);
    Rest = [X|Xs].

split(0, Xs, [], Xs).
split(N, [X|Xs], [X|Taken], Dropped) :-
    N #> 0,
    N1 #= N - 1,
    split(N1, Xs, Taken, Dropped).

apply_each(_Pred, []).
apply_each(Pred, [H|T]) :- call(Pred, H), apply_each(Pred, T).

% If V is a var, then call Expr, otherwise, reverse the order of the operations in Expr, then call it
call_or_inverse(V, Expr) :-
    call(V) -> call(Expr);

    Expr =.. [',' | _] ->
        reverse_conjunction(Expr, Reversed),
        call(Reversed);

    call(Expr).

range(A, B, Ns) :- findall(N, between(A, B, N), Ns).

% By default the format is the same as the extension, but all lowercase.
% Can be extended with your specific case if necessary.
line(Line, Stream) :-
    read_line_to_codes(Stream, Codes),
    atom_codes(Line, Codes);

    line(Line, Stream).

call_if_var(Pred, V) :-
    var(V) -> call(Pred);
    true.

next_matching(Phrase, Stream) :-
    line(Line, Stream),
    parse(Phrase, Line) -> true;

    next_matching(Phrase, Stream).

parse(Phrase, A) :-
    atom_codes(A, Codes),
    phrase(Phrase, Codes).

count_atom(Atom, Search, C) :-
    atomic_list_concat(Split, Search, Atom),
    length(Split, C).

surround_atom(Left, Right, A, B) :-
    nonvar(B),
    atom_concat(LeftPart, RightPart, B),
    atom_concat(Left, ALeft, LeftPart),
    atom_concat(ARight, Right, RightPart),
    atom_concat(ALeft, ARight, A).
surround_atom(Left, Right, A, B) :-
    nonvar(Left), nonvar(Right), nonvar(A),
    atom_concat(Left, A, Temp),
    atom_concat(Temp, Right, B).

% Delete everything
delete_cache :-
    cache_path('Test', AcheloisPath, _),
    exists_directory(AcheloisPath),
    delete_directory_and_contents(AcheloisPath).

% Delete just one part of the cache
delete_cache(BasePath) :-
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
    read_process(path(which), [ExeName], [output(TempPath)]),
    atomic_list_concat([Path|_], '\n', TempPath).

process(Exe, Args) :- process(Exe, Args, []).
process(Exe, Args, Options) :-
    (
        member(path(Path), Options);
        Path = '.'
    ),

    (
        member(exit_code(ExitCode), Options);
        true
    ),

    (
        member(stream(Stream), Options) -> process_stream(Stream, Exe, Args, Options);

        member(output(Output), Options) -> read_process(Path, Exe, Args, Output, ExitCode);

        run_process(Path, Exe, Args, ExitCode)
    ).

process_stream(Stream, Exe, Args, Options) :-
    (
        member(path(Path), Options);
        Path = '.'
    ),

    (
        member(pid(PID), Options);
        true
    ),

    process_create(Exe, Args, [cwd(Path), stdout(pipe(Stream)), stderr(pipe(Stream)), process(PID), detached(true)]).

run_process(Exe, Args) :- run_process('.', Exe, Args).
run_process(Path, Exe, Args) :- run_process(Path, Exe, Args, _).
run_process(Path, Exe, Args, ExitCode) :-
    setup_call_cleanup(
        process_create(Exe, Args, [cwd(Path), process(PID), detached(true)]),
        true,
        process_wait(PID, exit(ExitCode))).

read_process(Exe, Args, Output) :- read_process('.', Exe, Args, Output).
read_process(Path, Exe, Args, Output) :- read_process(Path, Exe, Args, Output, _).
read_process(Path, Exe, Args, Output, ExitCode) :-
    setup_call_cleanup(
        process_create(Exe, Args, [stdout(pipe(OutputStream)), stderr(pipe(OutputStream)), cwd(Path), process(PID), detached(true)]),
    (
        read_string(OutputStream, _, OutputStr),
        atom_string(Output, OutputStr)
    ),
    (
        process_wait(PID, exit(ExitCode)),
        close(OutputStream)
    )).

read_file(Path, Contents) :-
    setup_call_cleanup(
        open(Path, read, Stream),
        (
            read_string(Stream, _, String),
            atom_string(Contents, String)
        ),
        close(Stream)).

read_file_lines(Path, Lines) :-
    read_file(Path, Atom),
    atomic_list_concat(Lines, '\n', Atom).

write_file(OutputPath, Str) :-
    open(OutputPath, write, Stream),
    write(Stream, Str),
    close(Stream).

always(_).

take_while(_, [], []).
take_while(Pred, [H|T], [H|Rest]) :- call(Pred, H), take_while(Pred, T, Rest).
take_while(_, _, []).

take(_, [], []).
take(0, _, []).
take(N, [H|T], [H|Rest]) :-
    N #>= 0,
    N1 #= N - 1,
    take(N1, T, Rest),
    length(Rest, N1).

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

chars_of_type(Type, Chars) :-
    findall(C, char_type(C, Type), Temp),
    sort(Temp, Chars).

numbers(Numbers) :- chars_of_type(digit, Numbers).
letters(Letters) :-
    letters_lower(LowerLetters),
    letters_upper(UpperLetters),
    append(LowerLetters, UpperLetters, Letters).
letters_lower(LowerLetters) :- atom_chars('abcdefghijklmnopqrstuvwxyz', LowerLetters).
letters_upper(UpperLetters) :- atom_chars('ABCDEFGHIJKLMNOPQRSTUVWXYZ', UpperLetters).


unique(L) :- unique(L, L).

unique([], []).
unique([H|T], [H|Rest]) :-
    findall(X, (member(X, T), dif(X, H)), Temp),
    unique(Temp, Rest).

