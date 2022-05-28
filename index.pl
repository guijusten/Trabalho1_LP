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

remove_first([_ | T], T).

remove_last([_], []).
remove_last([H | T], [H | NoLast]) :- without_last(T, NoLast).

member(X,[X|_]).
member(X,[Y|T]) :- member(X,T).

first_elt([H | _], H).


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


encript_cesar(Input, X) :- string_to_list(Input, Output), convert_cesar(Output, S), name(X, S).

convert_cesar([H | T], [H1 | T1]) :- H1 is H + 1, convert_cesar(T, T1), !.
convert_cesar(_, []) :- !.


% Coisas úteis de saber 
% string_to_list('abcd', S). -> S = [97, 98, 99, 100].
% name(X, [65, 112]). -> X = 'Ap'.

% check_sorted([]).
% check_sorted([_]).
% check_sorted([Head, TailHead | Tail]) :- Head =< TailHead, check_sorted([TailHead | Tail]).