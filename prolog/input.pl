:- module(input, [screen_size/2]).

:- use_module(library(dcg/basics)).

:- use_module(utility).

% TODO Make everything windows compatible
%
screen_size(Width, Height) :-
    process(path(xdpyinfo), [], [output(Output)]),
    atomic_list_concat(Lines, '\n', Output),
    member(L, Lines),
    parse(input:size_str(Width, Height), L).

size_str(Width, Height) -->
    blanks, "dimensions:", blanks, integer(Width), "x", integer(Height), string(_).

screenshot(Img) :- screenshot(_, _, _, _, Img).
screenshot(X, Y, Width, Height, Img) :-
    call_if_var(
        (
            tmp_file('screenshot', Temp),
            atom_concat(Temp, '.bmp', Img)
        ), Img),
    call_if_var(X = 0, X),
    call_if_var(Y = 0, Y),
    call_if_var(screen_size(Width, _), Width),
    call_if_var(screen_size(_, Height), Height),

    format(atom(CropArg), '~wx~w+~w+~w', [Width, Height, X, Y]),

    process(path(import), ['-window', 'root', Img, '-crop', CropArg]).

% click_events(click(X, Y, Button)) :-
%     setup_call_cleanup(
%         process(path(xinput), ['test-xi2', '--root'], [stream(Stream), pid(PID)]),
%         (
%             line(Line, Stream),
%             Line = 'EVENT type 5 (ButtonRelease)',
%             line(Line, Stream),
%             parse(input:coords('root', X, Y), Line)
%         ),
%         (
%             close(Stream),
%             process_kill(PID)
%         )).

% coords(Name, X, Y) -->
%     blanks, string(Name), ": ", float(TX), "/", float(TY), string(_),
%     {
%         X is round(TX),
%         Y is round(TY)
%     }.

