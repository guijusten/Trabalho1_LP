man(gui).
man(marcos).
man(tom).
woman(mary).
woman(pat).

parent(gui, tom).

father(X, Y) :- parent(X, Y), man(X).



% Old Vigenere code

encript_vigenere(InputS, Key, X) :-
  string_to_list(InputS, CodeList),
  string_to_list(Key, KeyList),
  convert_vigenere(CodeList, EncriptedCodeList, KeyList),
  name(X, EncriptedCodeList).

convert_vigenere([IH | IT], [RH | RT], [KH | KT]) :-
  RH is IH + KH,
  convert_vigenere(IT, RT, KT),
  !.

convert_vigenere(_, [], _) :- !.



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