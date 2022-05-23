code('a', 1).
code('b', 2).
code('c', 3).
code('d', 4).
code('e', 5).
code('f', 6).
code('g', 7).
code('h', 8).
code('i', 9).
code('j', 10).
code('k', 11).
code('l', 12).
code('m', 13).
code('n', 14).
code('o', 15).
code('p', 16).
code('q', 17).
code('r', 18).
code('s', 19).
code('t', 20).
code('u', 21).
code('v', 22).
code('w', 23).
code('x', 24).
code('y', 25).
code('z', 26).

% Funções auxiliares
concatenate([], L, L).
concatenate([H | T], L, [H | Z]) :- concatenate(T, L, Z).

% code2string(S, [1, 2, 3])
code2string([], [H | T]) :- code(X, H), code2string([X], T).
code2string([X | S], [H | T]) :- code(X, H), code2string(S, T), !.
code2string([], S).

% string2code('abc', L)
string2code([H | T], []) :- code(H, X), string2code(T, [X]).
string2code([H | T], [X | S]) :- code(H, X), string2code(T, S), !.
string2code(L, []).

