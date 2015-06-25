:- module(ancestor,[father/2]).
:- dynamic father/2.

ancestor(X,Y):- father(X,Y).
ancestor(X,Y):- father(X,Z),ancestor(Z,Y).