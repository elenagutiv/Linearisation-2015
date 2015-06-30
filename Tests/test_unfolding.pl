z(X, Y) :- a(X, Y),a(Y, X).
a(X, Y) :- b(X, Z),c(Z, Y).
a(X, Y) :- d(X, Y),e(X, Y).
b(X, Y) :- f(X, Y).