:- module(image, [image_file/2]).

:- use_module(library(clpfd)).

:- use_module(bytes).
:- use_module(path).
:- use_module(utility).

% NOTE: currently not working

image_file(image(Format, Pixels), Path) :-
    nonvar(Pixels) ->
        open(Path, write, Stream, [type(binary)]),
        write_image(Format, Stream, Pixels),
        close(Stream);

    file_format(Path, Format),
    open(Path, read, Stream, [type(binary)]),
    read_image(Format, Stream, Pixels),
    close(Stream).

write_image(bmp, Stream, Pixels) :-
    length(Pixels, Height),
    Pixels = [Row|_],
    length(Row, Width),

    Stride #= Width * 3 + (4 - (Width * 3) mod 4) mod 4,
    FileSize #= 54 + 3 * Height * Stride,

    reverse(Pixels, Reversed),
    maplist(bmp_row, PixelData, Reversed),
    maplist(pad_end(Stride, 0), PixelData, Padded),
    flatten(Padded, Bytes),

    byte_groups(Stream,
        [
            chars(2, ['B', 'M']), int(4, FileSize), int(4, 0), int(4, 54),
            int(4, 40), int(4, Width), int(4, Height), int(2, 1),
            int(2, 24), int(4, 0), int(4, 0), int(4, 0), int(4, 0),
            int(4, 0), int(4, 0), bytes(Bytes)
        ]).

read_image(bmp, Stream, Pixels) :-
    byte_groups(Stream,
    [
        bytes(2, _BM), int(4, _FileSize), bytes(4, _Reserved), int(4, Offset),
        int(4, _HeaderSize), int(4, Width), int(4, _Height), bytes(24, _Information)
    ]),

    DataStart #= Offset - 54, % We've already read the 54 bytes for the header and information, so reduce the offset somewhat
    byte_group(Stream, skip(DataStart)), % Skip some bytes

    read_all_bytes(Stream, Bytes),
    length(Bytes, L),
    writeln(L),
    read_bmp_rows(Width, Bytes, TempRows),
    reverse(TempRows, Pixels).

read_bmp_rows(_Width, [], []).
read_bmp_rows(Width, Bytes, Rows) :-
    Stride #= Width * 3 + (Width * 3 mod 4),
    group(Stride, Bytes, Groups),
    ByteLength #= Width * 3,
    maplist(take(ByteLength), Groups, PixelData),
    maplist(bmp_row, PixelData, Rows).

bmp_row([], []).
bmp_row([B,G,R|Bytes], [pixel(R, G, B) | Pixels]) :- bmp_row(Bytes, Pixels).

