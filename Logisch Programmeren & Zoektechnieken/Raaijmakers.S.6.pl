% Prolog Homework #6
% 24 oktober 2016
% Steven Raaijmakers, 10804242, sjraaijmakers@gmail.com

% Question 1:

% Tableau implemented by Ulle Endriss (add iff myself):
:-  op(100, fy, neg),
    op(200, yfx, and),
    op(300, yfx, or),
    op(400, yfx, iff).

:-  op(600, xfx, ::).

tableau(_, Atoms) :- % don’t need to know Forms
    member(t :: A, Atoms),
    member(f :: A, Atoms), !. % no need to backtrack

% Negations:
tableau([t :: neg A | Forms], Atoms):-
    !,
    tableau([f :: A | Forms], Atoms).

tableau([f :: neg A | Forms], Atoms):-
    !,
    tableau([t :: A | Forms], Atoms).

% And (Conjunction):
tableau([t :: A and B | Forms], Atoms) :- !,
    tableau([t :: A, t :: B | Forms], Atoms).

tableau([f :: A and B | Forms], Atoms):-
    !,
    tableau([f :: A | Forms], Atoms),
    tableau([f :: B | Forms], Atoms).

% Or (Disjunction)
tableau([t :: A or B | Forms], Atoms) :- !,
    tableau([t :: A | Forms], Atoms),
    tableau([t :: B | Forms], Atoms).

tableau([f :: A or B | Forms], Atoms):-
    !,
    tableau([f :: A, f :: B | Forms], Atoms).

% iff (Biconditional):
tableau([t :: A iff B | Forms], Atoms) :- !,
    tableau([t :: A, t :: B| Forms], Atoms),
    tableau([t :: neg A, t :: neg B | Forms], Atoms).

tableau([f :: A iff B | Forms], Atoms) :- !,
    tableau([f :: A, f :: neg B| Forms], Atoms),
    tableau([t :: neg A, t :: B | Forms], Atoms).

%
tableau([Label :: Atom | Forms], Atoms):-
    atom(Atom), !,
    tableau(Forms, [Label :: Atom | Atoms]).

tautology(Formula):-
    tableau([f :: Formula], []).

% Examples:
% ?- tautology((p  and q) iff (q or p)).
% false.
%
% ?- tautology((p  and q) iff (q and p)).
% true.

% Question 2:

% Checks wheter Arg1 |= Arg2. Same as % Arg1, (Arg2 --> F) |= False
% Completes whenever Arg1 and Arg2 are not equal.
% So if Arg1 is true Arg2 must be false, and viceversa
entails(Arg1, Arg2):-
    !,
    tableau([t :: Arg1, f :: Arg2], []).

entails(Arg1, Arg2):-
    !,
    tableau([f :: Arg1, t :: Arg2], []).

% Examples (first is demorgan)
% ?- entails(neg(p and q), neg p or neg q).
% true.
%
% ?- entails(neg(p and q), neg p or q).
% false.

% Question 3:
% Checks if first argument occurs in a way in the second argument:

% Helpers:
% If Term matches X:
occurs_h(X, Term):-
    X == Term.

% No match: recursive call to only the Terms X (without function f)
occurs_h(X, Term):-
    occurs(X, Term), !.

% Occurs splits function f(y) into f and y, and checks if y contains the first
% arg X. If not check if it is possible to split y up into a function and a term
% and repeat.
occurs(X, Function):-
    \+ var(Function),
    Function =.. [_ | Terms],
    member(Term, Terms),
    occurs_h(X, Term).

% Examples:
% ?- occurs(X, f(Y,g(y, (g(X))))).
% true.
%
% ?- occurs(X, f(Y,g(y, (g(B))))).
% false.

% Question 4:
