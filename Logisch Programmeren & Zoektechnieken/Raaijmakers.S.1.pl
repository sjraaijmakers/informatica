/* Steven Raaijmakers 10804242 */

/* Sources: */
/* http://stackoverflow.com/questions/10338666/prolog-power-predicate */
/* http://stackoverflow.com/questions/2260792/remove-duplicates-in-list-prolog */
/* http://okmij.org/ftp/Prolog/Arithm/unary.prl */

/* 1: answers gets the TA of student X, and the TA's favourite topping */

has_ta(alice, mary).
has_ta(bob, mary).
has_ta(cecilia, paul).
has_ta(steven, rex).

has_favourite_toping(peter, nutella).
has_favourite_toping(paul, 'strawberry jam').
has_favourite_toping(mary, caramel).
has_favourite_toping(rex, sprinkles).

answer(X) :-
    has_ta(X, Y),
    has_favourite_toping(Y, Z),
    write("The favourite pancake topping of the TA of this student is "), write(Z),
    write("."), nl.

/* The favourite pancake topping of the TA of this student is sprinkles. */

/* 2: returns the head and tail of given list */

analyse_list([Head | Tail]) :-
    write("The head of your list is: "), write(Head), nl,
    write("The tail of your list is: "), write(Tail), nl.
analyse_list([]) :- write("This is an empty list"), nl.

/* 3: removes duplicates from given list */

remove_duplicates([], []).
remove_duplicates([Head | Tail], [Head | Output]) :-
    not(member(Head, Tail)),
    remove_duplicates(Tail, Output).
remove_duplicates([Head | Tail], Output) :-
    member(Head, Tail),
    remove_duplicates(Tail, Output).

/* 4: multiple operations for the unary system. */

successor(Input, Output) :-
    append(Input, [x], Output).

plus(X, Y, Output) :-
    append(X, Y, Output).

times([], _, []).
times([x | _], [], []).
times([x | X], [x | Y], Output):-
	plus([x | Y], Z1, Output),
	times(X, [x | Y], Z1).

/* 5: returns all subsets of given list/set */

power([], []).
power([Tmp | Tail], [Tmp | Tail2]):-
    power(Tail, Tail2).
power([_ | Tail], Tail2):-
    power(Tail, Tail2).
