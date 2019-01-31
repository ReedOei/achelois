:- module(bytes, [bytestream/2, bytestream_int/3, read_all_bytes/2, byte_group/2, byte_groups/2]).

:- use_module(math).
:- use_module(utility).

byte_groups(Stream, Groups) :- apply_each(bytes:byte_group(Stream), Groups).

byte_group(Stream, skip(N)) :- byte_group(Stream, bytes(N, _)).
byte_group(Stream, bytes(Bytes)) :- byte_group(Stream, bytes(_, Bytes)).
byte_group(Stream, atom(A)) :- byte_group(Stream, atom(_, A)).
byte_group(Stream, atom(N, A)) :-
    ( nonvar(N), var(A) -> length(Bytes, N); true ),
    call_or_inverse(nonvar(A),
    (
        atom_chars(A, Chars),
        maplist(char_code, Chars, Bytes),
        length(Bytes, N),
        bytestream(Stream, Bytes)
    )).
byte_group(Stream, chars(N, Chars)) :-
    length(Chars, N),
    same_length(Chars, Bytes),
    call_or_inverse(nonvar(Chars),
    (
        maplist(char_code, Chars, Bytes),
        bytestream(Stream, Bytes)
    )).
byte_group(Stream, bytes(N, Bytes)) :-
    nonvar(N) -> integer(N),
    length(Bytes, N),
    bytestream(Stream, Bytes).
byte_group(Stream, int(N, Int)) :- bytestream_int(N, Stream, Int).

bytestream(Stream, Bytes) :-
    stream_property(Stream, output) -> apply_each(put_byte(Stream), Bytes);
    stream_property(Stream, input) ->
    (
        nonvar(Bytes) -> apply_each(get_byte(Stream), Bytes);

        read_all_bytes(Stream, Bytes)
    ).

bytestream_int(ByteCount, Stream, N) :-
    range(0, 255, Digits),
    (
        nonvar(N) ->
            to_digits(256, Digits, N, Temp),
            pad_begin(ByteCount, 0, Temp, Padded),
            reverse(Padded, Bytes),
            bytestream(Stream, Bytes);

        length(Temp, ByteCount),
        bytestream(Stream, Temp),
        reverse(Temp, Bytes),
        from_digits(256, Digits, Bytes, N)
    ).

read_all_bytes(Stream, Bytes) :-
    read_string(Stream, _, Str),
    string_codes(Str, Bytes).

