:- module(latex, [new_command/2]).

new_command(Name, Value, Command) :-
    atomic_list_concat(['\\newcommand{\\', Name, '}{', Value, '\\xspace}'], '', Command).

