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

word(abacate).
word(animal).
word(bacia).
word(brinco).
word(colar).
word(criatura).
word(dirigente).
word(disciplina).
word(esquisito).
word(epidemia).
word(forma).
word(findar).
word(girassol).
word(gastronomia).
word(hieroglifo).
word(hidrante).
word(indagar).
word(indicar).
word(janta).
word(jumento).
word(liberar).
word(labirinto).
word(misterioso).
word(magia).
word(natural).
word(noite).
word(ostra).
word(ocultar).
word(participar).
word(pedra).
word(quarto).
word(quinta).
word(retroceder).
word(runa).
word(socar).
word(sinalizar).
word(ter).
word(totalidade).
word(urubu).
word(uivar).
word(viajar).
word(vacilar).
word(xilofone).
word(xadrez).
word(zebra).
word(zumbido).


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Inserir palavras na base de dados

:- use_module(library(persistency)).

:- persistent word(word).

:- initialization(init).

init:-
  absolute_file_name('database.db', File, [access(write)]),
  db_attach(File, []).


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Funções auxiliares

concatenate([], L, L).
concatenate([H | T], L, [H | Z]) :- concatenate(T, L, Z).

remove_first([_ | T], T).

remove_last([_], []).
remove_last([H | T], [H | NoLast]) :- without_last(T, NoLast).

member(X,[X | _]).
member(X,[_ | T]) :- member(X,T).

first_elt([H | _], H).

check_if_zero(0, 1).
check_if_zero(N, N).

list_length([], 0).
list_length([_ | T], N) :- length(T, N1), N is N1 + 1.


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


% code2string(S, [1, 2, 3])
code2string([], [H | T]) :- 
  code(X, H), 
  code2string([X], T).

code2string([X | S], [H | T]) :- 
  code(X, H), 
  code2string(S, T), !.

code2string([], _).

% string2code('abc', L)
string2code(X, L) :-
    atom_chars(X, List),
    aux_string2code(List, L).

aux_string2code([H | T], []) :- 
  code(H, X), 
  aux_string2code(T, [X]).

aux_string2code([H | T], [X | S]) :- 
  code(H, X), 
  aux_string2code(T, S), !.

aux_string2code(_, []).


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Cifra de César


% Encriptando uma palavra com uma chave usando Cifra de César
encript_cesar(InputS, InputN, Result) :- 
    string2code(InputS, CodeList), 
    convert_cesar(CodeList, EncriptedCodeList, InputN), 
    code2string(X, EncriptedCodeList),
    atom_chars(Result, X).

convert_cesar([H | T], [H1 | T1], InputN) :- 
    Aux is mod(H + InputN, 26),
    check_if_zero(Aux, H1),
    convert_cesar(T, T1, InputN), 
    !.

convert_cesar(_, [], _) :- !.


% Inserindo uma palavra encriptada por Cifra de César no DB
assert_encripted_cesar(InputS, InputN) :-
  encript_cesar(InputS, InputN, Output),
  assert_word(Output).

% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Cifra de Vigenere


% Encriptando uma palavra com uma chave usando Cifra de Vigenère
encript_vigenere(InputS, Key, Result) :-
  string2code(InputS, CodeList),
  string2code(Key, KeyList),
  list_length(KeyList, KeyLength),
  convert_vigenere(CodeList, EncriptedCodeList, KeyList, KeyLength, 0),
  code2string(X, EncriptedCodeList),
  atom_chars(Result, X).

convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, KeyLength) :-
  convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, 0).

convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, I) :-
  nth0(I, KeyList, KeyValue),
  RH is mod(IH + KeyValue, 26),
  I2 is I + 1,
  convert_vigenere(IT, RT, KeyList, KeyLength, I2),
  !.

convert_vigenere(_, [], _, _, _) :- !.


% Inserindo uma palavra encriptada por Cifra de Vigenère no DB
assert_encripted_vigenere(InputS, Key) :-
  encript_vigenere(InputS, Key, Output),
  assert_word(Output).


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Inserindo palavras encriptadas na base de dados

assert_encripted_cesar(anomalia, 5).
assert_encripted_cesar(cardiologia, 9).
assert_encripted_vigenere(documento, ler).
assert_encripted_cesar(entrada, 1).
assert_encripted_vigenere(fantasia, som).
assert_encripted_cesar(gigante, 2).
assert_encripted_vigenere(celeiro, obter).
assert_encripted_cesar(idolatria, 19).
assert_encripted_vigenere(gelatina, doce).
assert_encripted_cesar(lobo, 25).
assert_encripted_vigenere(pedregulho, filho).
assert_encripted_cesar(nuvem, 5).
assert_encripted_vigenere(natureza, vida).
assert_encripted_cesar(pertencimento, 21).
assert_encripted_vigenere(documento, ler).
assert_encripted_cesar(rotina, 12).
assert_encripted_vigenere(mitologia, grega).
assert_encripted_cesar(troglodita, 7).
assert_encripted_vigenere(vermelho, mar).
assert_encripted_cesar(vassoura, 3).
assert_encripted_vigenere(escudo, espada).
assert_encripted_cesar(astrologia, 15).
assert_encripted_vigenere(batalha, jogo).
assert_encripted_cesar(entropia, 24).
assert_encripted_vigenere(canil, osso).
assert_encripted_cesar(boliche, 18).
assert_encripted_vigenere(viralizar, fim).
assert_encripted_cesar(livraria, 8).
assert_encripted_vigenere(ferrovia, trem).


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Coisas úteis de saber

% string_to_list('abcd', S). -> S = [97, 98, 99, 100].
% name(X, [65, 112]). -> X = 'Ap'.

% check_sorted([]).
% check_sorted([_]).
% check_sorted([Head, TailHead | Tail]) :- Head =< TailHead, check_sorted([TailHead | Tail]).


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Problemas

% encript_cesar: Se eu passar um número tal que o código do caracter + InputNumber seja > 26, dá merda.
% Ele atribui um caracter estranho para a string.
% 
% Tenho somente 46 palavras na base de dados. Preciso de no minimo 100
% 