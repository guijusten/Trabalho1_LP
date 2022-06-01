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



