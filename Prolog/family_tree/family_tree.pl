parent(tom, bob).
parent(bob, pat).
parent(bob, ada).
parent(pat, jim).
parent(ada, michelle).

male(tom).
male(bob).
male(jim).
male(pat).
female(ada).
female(michelle).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
brother(X, Y) :- male(X), sibling(X, Y).
aunt(X, Y) :-  female(X),parent(Z,Y),sibling(X,Z).
cousin(X,Y):- parent(Px,X),parent(Py,Y),sibling(Px,Py).
grandparent(X,Y):-parent(Py,Y),parent(X,Py).
descendent(X,Y):-parent(Y,X).
descendent(X,Y):-parent(P,X),descendant(P,Y).
