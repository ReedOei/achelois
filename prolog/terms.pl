:- module(terms, []).

replace(A, B, A, B).
replace(A, _B, C, C) :- dif(A, C).

replace_expr(A, B, A, B).
replace_expr(A, B, Expr, NewExpr) :-
    Expr =.. [V],
    replace(A, B, V, NewV), !,
    NewExpr =.. [NewV].
replace_expr(A, B, Expr, NewExpr) :-
    Expr =.. List,
    List = [_,_|_],
    maplist(replace_expr(A, B), List, TempList),
    maplist(replace(A, B), TempList, NewList), !,
    NewExpr =.. NewList.

replace_expr_list([], _, Expr, Expr).
replace_expr_list(_, [], Expr, Expr).
replace_expr_list([V|Vs], [R|Rs], Expr, NewExpr) :-
    replace_expr(V, R, Expr, TempExpr), !,
    replace_expr_list(Vs, Rs, TempExpr, NewExpr).

replace_equiv_list(Vs, Rs, A=B, NewA=NewB) :-
    replace_expr_list(Vs, Rs, A, NewA),
    replace_expr_list(Vs, Rs, B, NewB).

expr_vars(Expr, [Expr]) :-
    functor(Expr, _, N),
    N = 0.
expr_vars(Expr, Vars) :-
    functor(Expr, _, N),
    N > 0,
    Expr =.. [_|Args],
    maplist(expr_vars, Args, AllVars),
    flatten(AllVars, Vars).

term_length(A, Length) :-
    term_to_atom(A, Atom),
    atom_codes(Atom, Codes),
    length(Codes, Length).

shorter(A, B, Shorter) :-
    term_length(A, LenA),
    term_length(B, LenB),
    (
        LenA < LenB,
        Shorter = A;

        LenB < LenA,
        Shorter = B
    ).

sub_exprs(Expr, Subs) :-
    Expr =.. [_|T],
    (
        T = [],
        SubExprs = [];

        T = [_|_],
        maplist(sub_exprs, T, AllSubExprs),
        flatten(AllSubExprs, Temp),
        append(T, Temp, SubExprs)
    ),
    sort([Expr|SubExprs], Subs).

