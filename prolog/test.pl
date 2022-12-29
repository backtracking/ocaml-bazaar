
even(o).

even(s(s(X))) :- even(X).

?- even(s(o)).
