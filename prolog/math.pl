:- module(math, [factorial/2, base_digits/2, base_conv/4, to_base_10/3,
                 to_digits/2, to_digits/3, to_digits/4,
                 from_digits/2, from_digits/4, from_digits/5,
                 lcm/3, gcd/3, first_numbers/2, first_numbers/3, sumf/2, averagef/2,
                 seq/2, arith_seq/2, geom_seq/2, increasing/1]).

:- use_module(library(clpfd)).

:- use_module(utility).

factorial(0, 1).
factorial(N, F) :-
    N1 #= N - 1,
    factorial(N1, F1),
    F #= N * F1.

base_digits(Base, BaseDigits) :-
    numbers(Numbers),
    letters_lower(LowerLetters),
    letters_upper(UpperLetters),
    flatten([Numbers, LowerLetters, UpperLetters], DigitList),
    take(Base, DigitList, BaseDigits).

from_digits(NumDigits, Out) :-
    base_digits(10, FromDigits),
    from_digits(10, FromDigits, NumDigits, Out).
from_digits(FromBase, FromDigits, NumDigits, Out) :-
    foldl(from_digits(FromBase, FromDigits), NumDigits, 0, Out).
from_digits(Base, BaseDigits, Digit, Cur, Out) :-
    nth0(DigitVal, BaseDigits, Digit),
    Out #= Base * Cur + DigitVal.

to_digits(N, Digits) :- base_digits(10, Ds), to_digits(10, Ds, N, Digits).
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

    from_digits(FromBase, FromDigits, NAtom, Base10),

    to_digits(ToBase, Base10, MAtom),
    atom_chars(M, MAtom).

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

lcm(A, B, L) :-
    gcd(A, B, G),
    L * G #= A * B.

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


