% Codificações de caracteres e palavras

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

% Funções auxiliares

concatenate([], L, L).
concatenate([H | T], L, [H | Z]) :- concatenate(T, L, Z).

remove_first([_ | T], T).

remove_last([_], []).
remove_last([H | T], [H | NoLast]) :- without_last(T, NoLast).

member(X,[X | _]).
member(X,[_ | T]) :- member(X,T).

first_elt([H | _], H).

list_length([], 0).
list_length([_ | T], N) :- length(T, N1), N is N1 + 1.


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Cifra de César

encript_cesar(InputS, InputN, X) :- 
    string_to_list(InputS, CodeList), 
    convert_cesar(CodeList, EncriptedCodeList, InputN), 
    name(X, EncriptedCodeList).

convert_cesar([H | T], [H1 | T1], InputN) :- 
    H1 is H + InputN,
    convert_cesar(T, T1, InputN), 
    !.

convert_cesar(_, [], _) :- !.


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Cifra de Vigenere

encript_vigenere(InputS, Key, X) :-
  string_to_list(InputS, CodeList),
  string_to_list(Key, KeyList),
  list_length(KeyList, KeyLength),
  convert_vigenere(CodeList, EncriptedCodeList, KeyList, KeyLength, 0),
  name(X, EncriptedCodeList).

convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, KeyLength) :-
  convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, 0).

convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, I) :-
  nth0(I, KeyList, KeyValue),
  RH is IH + KeyValue,
  I2 is I + 1,
  convert_vigenere(IT, RT, KeyList, KeyLength, I2),
  !.

convert_vigenere(_, [], _, _, _) :- !.


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Inserir palavras na base de dados

:- use_module(library(persistency)).

:- persistent word(word).

:- initialization(init).

init:-
  absolute_file_name('fact.db', File, [access(write)]),
  db_attach(File, []).


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
% No momento estou usando o string_to_list e name, mas preciso fazer o code2string e o string2code, 
% que estão em arquivo separado
% 
% No momento, vigenere está correto, mas se eu encriptar 'abc' com a chave 'abc', vou ter a soma 97 + 97, 
% que dá merda
% 
% Provavelmente vou ter problemas no vigenere quando a chave for menor que a palavra a ser encriptada 
% Para isso, quero pegar o length da lista, e a cada iteração, aumentar em 1 o valor de uma variável.
% Qnd for codificar o caracter, pegar o elemento na posição L[variável]
% Qnd essa variável == length, zerá-la.
% 