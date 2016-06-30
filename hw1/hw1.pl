%% Assignment 1 - 1/17/16 - Aidan Gadberry agadberr

father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

grandma(X,Y) :-
	mother(X,Z), mother(Z,Y);
	mother(X,Z), father(Z,Y).

descendants(X,Y) :-
	mother(X,Y); father(X,Y); grandma(X,Y).

siblings(X,Y) :-
	mother(W,X), mother(W,Y), not(X=Y).

siblings(X,Y) :-
	father(Z,X), father(Z,Y), not(mother(W,X)), not(mother(W,Y)), not(X=Y).


transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).
accepting(q2).
accepting(q3).

accepts(State, []) :-
	accepting(State).
% base case

accepts(State, InputList) :-
	InputList = [H|T],
	transition(State, X, H),
	accepts(X, T).
