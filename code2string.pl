% code2string(S, [1, 2, 3])
code2string([], [H | T]) :- code(X, H), code2string([X], T).
code2string([X | S], [H | T]) :- code(X, H), code2string(S, T), !.
code2string([], S).

code2string2(S, []).
code2string2([H | T], [H1 | T1]) :- code(H, H1), test2(T, T1).
code2string2(S, List) :- remove_last(S, List).

% string2code('abc', L)
string2code([H | T], []) :- code(H, X), string2code(T, [X]).
string2code([H | T], [X | S]) :- code(H, X), string2code(T, S), !.
string2code(L, []).