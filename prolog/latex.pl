:- module(latex, [new_command/3]).

new_command(Name, Value, Command) :-
    atomic_list_concat(['\\newcommand{\\', Name, '}{', Value, '\\xspace}'], '', Command).

% Make this bidirectional (write to or read from a table).
table.

