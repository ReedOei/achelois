:- module(utility, [string_concat_list/2, intercalate/3, lookup_path/2,
                    read_process/3, read_process/4, read_process/5,
                    read_file/2, read_file_lines/2, write_file/2, list_empty/1,
                    list_files/2, run_process/2, run_process/3, run_process/4,
				    walk/2, take_while/3, take/3, cache/3, cache_global/3, nth_parent_dir/3,
                    delete_cache/0, delete_cache/1,
                    base_digits/2, base_conv/4, to_base_10/3,
                    chars_of_type/2, numbers/1, letters_lower/1, letters_upper/1, letters/1,
                    unique/1, unique/2,
                    sublist/2,
                    gcd/3, first_numbers/2, first_numbers/3, sumf/2, averagef/2,
                    seq/2, arith_seq/2, geom_seq/2, increasing/1, to_digits/2, to_digits/3, to_digits/4]).

:- use_module(library(filesex)).
:- use_module(library(clpfd)).

nth_parent_dir(0, Path, Path).
nth_parent_dir(N, Path, Parent) :-
    absolute_file_name(Path, AbsPath),
    file_directory_name(AbsPath, TempParent),
    N1 #= N - 1,
    nth_parent_dir(N1, TempParent, Parent).

factorial(0, 1).
factorial(N, F) :-
    N1 #= N - 1,
    factorial(N1, F1),
    F #= N * F1.

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
    read_process('.', path(which), [ExeName], TempPath),
    atomic_list_concat([Path|_], '\n', TempPath).

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
take(N, [H|T], [H|Rest]) :-
    N #>= 0,
    N1 #= N - 1,
    take(N1, T, Rest),
    length(Rest, N1).

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

base_digits(Base, BaseDigits) :-
    numbers(Numbers),
    letters_lower(LowerLetters),
    letters_upper(UpperLetters),
    flatten([Numbers, LowerLetters, UpperLetters], DigitList),
    take(Base, DigitList, BaseDigits).

from_digits(Base, BaseDigits, Digit, Cur, Out) :-
    nth0(DigitVal, BaseDigits, Digit),
    Out #= Base * Cur + DigitVal.

to_digits(N, Digits) :- base_digits(10, Ds), digits(10, Ds, N, Digits).
to_digits(Base, N, Digits) :-
    base_digits(Base, BaseDigits),
    to_digits(Base, BaseDigits, N, Digits).
to_digits(Base, BaseDigits, N, Digits) :-
    N #< Base,
    nth0(N, BaseDigits, NChar),
    Digits = [NChar];

    N #>= Base,
    DVal #= N mod Base,
    NewN #= N div Base,

    to_digits(Base, BaseDigits, NewN, Rest),
    nth0(DVal, BaseDigits, D),
    append(Rest, [D], Digits).

to_base_10(FromBase, N, M) :-
    base_conv(FromBase, 10, N, TempM),
    term_to_atom(M, TempM).

base_conv(FromBase, ToBase, N, M) :-
    base_digits(FromBase, FromDigits),

    (
        atom(N),
        atom_chars(N, NAtom);

        not(atom(N)),
        term_to_atom(N, TempAtom),
        atom_chars(TempAtom, NAtom)
    ),

    foldl(from_digits(FromBase, FromDigits), NAtom, 0, Base10),

    to_digits(ToBase, Base10, MAtom),
    atom_chars(M, MAtom).

unique(L) :- unique(L, L).

unique([], []).
unique([H|T], [H|Rest]) :-
    findall(X, (member(X, T), dif(X, H)), Temp),
    unique(Temp, Rest).

seq(_, []).
seq(_, [_]).
seq(Pred, [Start, Next|Rest]) :-
    call(Pred, Next, Start),
    seq(Pred, [Next|Rest]).

increasing([]).
increasing([_]).
increasing([A,B|Tail]) :-
    A #=< B,
    increasing([B|Tail]).

arith_seq_f(Inc, Next, Start) :- Next #= Start + Inc.
arith_seq(Inc, Seq) :- seq(arith_seq_f(Inc), Seq).

geom_seq_f(M, Next, Start) :- Next #= M * Start.
geom_seq(M, Seq) :- seq(geom_seq_f(M), Seq).

first_numbers(_, _, []).
first_numbers(Property, X, [X|T]) :-
    call(Property, X),
    Next #= X + 1,
    first_numbers(Property, Next, T).
first_numbers(Property, X, T) :-
    not(call(Property, X)),
    Next #= X + 1,
    first_numbers(Property, Next, T).

first_numbers(Property, Numbers) :- first_numbers(Property, 0, Numbers).

gcd(A, 0, A).
gcd(A, B, G) :-
    NewB #= A mod B,
    gcd(B, NewB, G).

sumf([], 0.0).
sumf([H|T], S) :-
    sumf(T, SumT),
    S is SumT + H.

averagef(Values, Average) :-
    sumf(Values, Sum),
    length(Values, L),
    Average is Sum / float(L).

