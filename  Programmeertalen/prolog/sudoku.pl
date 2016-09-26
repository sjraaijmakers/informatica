% Sudoku Solver. Door: Steven Raaijmakers (10804242).
% Bron: http://www.swi-prolog.org/pldoc/man?predicate=transpose/2
% Zie doc voor expliciete uitleg van dit programma.

% Library voor Tranpose
:- use_module(library(clpfd)).

% Sudoku functie
sudoku(Rows, Solution) :-
    append(Rows, Empty),
    Empty ins 1..9,

    maplist(all_distinct, Rows),
    transpose(Rows, Cols),
    maplist(all_distinct, Cols),

    Rows = [
    [A1, B1, C1, D1, E1, F1, G1, H1, I1],
    [A2, B2, C2, D2, E2, F2, G2, H2, I2],
    [A3, B3, C3, D3, E3, F3, G3, H3, I3],
    [A4, B4, C4, D4, E4, F4, G4, H4, I4],
    [A5, B5, C5, D5, E5, F5, G5, H5, I5],
    [A6, B6, C6, D6, E6, F6, G6, H6, I6],
    [A7, B7, C7, D7, E7, F7, G7, H7, I7],
    [A8, B8, C8, D8, E8, F8, G8, H8, I8],
    [A9, B9, C9, D9, E9, F9, G9, H9, I9]
    ],

    Blocks = [
    [A1, B1, C1, A2, B2, C2, A3, B3, C3],
    [D1, E1, F1, D2, E2, F2, D3, E3, F3],
    [G1, H1, I1, G2, H2, I2, G3, H3, I3],

    [A4, B4, C4, A5, B5, C5, A6, B6, C6],
    [D4, E4, F4, D5, E5, F5, D6, E6, F6],
    [G4, H4, I4, G5, H5, I5, G6, H6, I6],

    [A7, B7, C7, A8, B8, C8, A9, B9, C9],
    [D7, E7, F7, D8, E8, F8, D9, E9, F9],
    [G7, H7, I7, G8, H8, I8, G9, H9, I9]
    ],

    maplist(all_distinct, Blocks),

    Solution = Rows.
