%   Código feito por Guilherme Fiorini Justen, 201965041AC

% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Bijeção de letras para numerais 

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


% Palavras na base de dados

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
word(anomalia).
word(cardiologia).
word(ler).
word(documento).
word(entrada).
word(fantasia).
word(som).
word(gigante).
word(celeiro).
word(idolatria).
word(gelatina).
word(doce).
word(lobo).
word(pedregulho).
word(filho).
word(nuvem).
word(natureza).
word(vida).
word(pertencimento).
word(documento).
word(baqueta).
word(rotina).
word(mitologia).
word(grego).
word(troglodita).
word(vermelho).
word(mar).
word(vassoura).
word(escudo).
word(espada).
word(astrologia).
word(batalha).
word(jogo).
word(entropia).
word(canil).
word(osso).
word(boliche).
word(viralizar).
word(fim).
word(livraria).
word(ferrovia).
word(trem).
word(tomada).
word(touro).
word(flores).
word(mola).
word(desnutrir).
word(enquadrar).
word(logotipo).
word(torcida).
word(suspeitar).
word(pirotecnia).
word(tecnicalidade).
word(tampa).
word(chamado).
word(coringa).

% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Criando base de dados

:- use_module(library(persistency)).

:- persistent word(word).

:- initialization(init).

init:-
  absolute_file_name('database.db', File, [access(write)]),
  db_attach(File, []).


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% Predicados auxiliares

concatenate([], L, L).
concatenate([H | T], L, [H | Z]) :- concatenate(T, L, Z).

remove_first([_ | T], T).

remove_last([_], []).
remove_last([H | T], [H | NoLast]) :- without_last(T, NoLast).

member(X,[X | _]).
member(X,[_ | T]) :- member(X,T).

first_elt([H | _], H).

check_if_zero(0, 26).
check_if_zero(N, N).

list_length([], 0).
list_length([_ | T], N) :- length(T, N1), N is N1 + 1.

create_list_of_a(Len, List)  :- 
    length(List, Len), 
    maplist(=('a'), List).


% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% code2string
code2string([], [H | T]) :-
  code(X, H), 
  code2string([X], T).

code2string([X | S], [H | T]) :- 
  code(X, H), 
  code2string(S, T), !.

code2string([], _).


% string2code
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


% Encriptando uma palavra com uma chave
encript_cesar(InputS, InputN, Result) :- 
    string2code(InputS, CodeList), 
    convert_cesar(CodeList, EncriptedCodeList, InputN), 
    code2string(X, EncriptedCodeList),
    atom_chars(Result, X).

% Caso geral
convert_cesar([H | T], [H1 | T1], InputN) :- 
    Aux is mod(H + InputN, 26),
    check_if_zero(Aux, H1),
    convert_cesar(T, T1, InputN), 
    !.

% Critério de parada
convert_cesar(_, [], _) :- !.


% Decriptando uma palavra sem saber a chave
decript_cesar(InputS, X) :-
  aux_decript_cesar(InputS, X, 0).

% Caso tenha encontrado a palavra
aux_decript_cesar(InputS, InputS, _) :-
  word(InputS).

% Caso geral
aux_decript_cesar(InputS, X, Num) :-
  encript_cesar(InputS, 1, Result),
  Num < 26,
  NewNum is Num + 1,
  aux_decript_cesar(Result, X, NewNum).


% Inserindo uma palavra encriptada no DB
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

% Caso a chave chegue ao final
convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, KeyLength) :-
  convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, 0).

% Caso geral
convert_vigenere([IH | IT], [RH | RT], KeyList, KeyLength, I) :-
  nth0(I, KeyList, KeyValue),
  RH is mod(IH + KeyValue, 26),
  I2 is I + 1,
  convert_vigenere(IT, RT, KeyList, KeyLength, I2),
  !.

% Critério de parada
convert_vigenere(_, [], _, _, _) :- !.


% Inserindo uma palavra encriptada por Cifra de Vigenère no DB
assert_encripted_vigenere(InputS, Key) :-
  encript_vigenere(InputS, Key, Output),
  assert_word(Output).