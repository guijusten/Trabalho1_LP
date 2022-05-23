man(gui).
man(marcos).
man(tom).
woman(mary).
woman(pat).

parent(gui, tom).

father(X, Y) :- parent(X, Y), man(X).
