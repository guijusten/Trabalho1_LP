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

% code2string(S, [1, 2, 3])
code2string([], [H | T]) :- code(X, H), code2string([X], T).
code2string([X | S], [H | T]) :- code(X, H), code2string(S, T), !.
code2string([], S).

% string2code('abc', L)
aux_string2code([H | T], []) :- code(H, X), aux_string2code(T, [X]).
aux_string2code([H | T], [X | S]) :- code(H, X), aux_string2code(T, S), !.
string2code(X, L) :-
    atom_chars(X, List),
    aux_string2code(List, L).
aux_string2code(L, []).


code2string2(S, []).
code2string2([H | T], [H1 | T1]) :- code(H, H1), test2(T, T1).
code2string2(S, List) :- remove_last(S, List).

% • um predicado de aridade 3 que relaciona duas listas com uma terceira lista de pares, na qual cada par
% é formado por um elemento de cada uma das lista. Caso a segunda lista seja menor que a primeira,
% replica-se o texto até que tenha o mesmo tamanho. Por exemplo, suponha L1 = [a, b, c, d, e] e L2 =
% [f, l, a], a lista de pares deve ser L3 = [(a, f),(b, l),(c, a),(d, f),(e, l)]. Note que os elementos de L2
% foram replicados até ter o mesmo tamanho de L1, resultando na lista [f, l, a, f, l];

% • um predicado que relaciona uma mensagem cifrada, um tamanho de chave, uma palavra que sabida-
% mente ocorre na mensagem decifrada e sua posição, com a chave. Por simplificação, pode assumir que
% o tamanho da chave ́e menor que a palavra que ocorre no texto e que o texto;

% • um predicado que relaciona uma mensagem cifrada, um tamanho de chave e uma palavra que ocorre
% no texto com a mensagem decifrada;

% • um predicado que relaciona uma mensagem cifrada, uma lista de possíveis palavras que ocorre no texto
% e um tamanho de chave com a mensagem decifrada.

first_pred(L1, L2, Result) :-
  aux_first_pred(L1, L2, L2, Result).

aux_first_pred([], _, _, _).

aux_first_pred(L1, [], L2Copy, Result) :-
  aux_first_pred(L1, L2Copy, L2Copy, Result)

aux_first_pred([H1 | T1], [H2, | T2], L2Copy, [[H1, H2], T3]) :-
  aux_first_pred(T1, T2, L2Copy, ).

aux_first_pred([H1 | T1], [H2, | T2], L2Copy, [H3 | T3]) :-
  aux_first_pred(T1, T2, L2Copy, [[H1, H2], H3, T3]).


% Mais Novo

first_pred(L1, L2, Result) :-
  aux_first_pred(L1, L2, L2, Result).

aux_first_pred([], _, _, _).

aux_first_pred([H1 | T1], [H2, | T2], L2Copy, [[H1, H2] | T3]) :-
  aux_first_pred(T1, T2, L2Copy, T3).

aux_first_pred([H1 | T1], [H2, | T2], L2Copy, _) :-
  aux_first_pred(T1, T2, L2Copy, [[H1, H2]]).