
even(o).
even(s(s(N))) :- even(N).

append(nil, L, L).
append(cons(X,L1), L2, cons(X,L12)) :- append(L1, L2, L12).

length(nil, o).
length(cons(X,L), s(N)) :- length(L, N).

member(X, cons(X,L)).
member(X, cons(Y,L)) :- member(X,L).

?- even(s(o)).
?- even(s(s(s(s((o)))))).

?- append(cons(a,nil), cons(b,nil), L).

?- member(a, cons(
