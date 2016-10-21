% Prolog Homework #5
% 18 oktober 2016
% Steven Raaijmakers, 10804242, sjraaijmakers@gmail.com

% Question 1:

% When one of the elements of terms evaluates to Target
goal(Target:Terms):-
    member(X, Terms),
    X =:= Target.

% Example:
% ?- goal(200:[5, 10*20, 1])
% true.

% When operation is division, special handling:
move(Terms, /, [Ar | Tmp2]):-
    select(Term1, Terms, Tmp),
    select(Term2, Tmp, Tmp2),
    Ar =.. [/, Term1, Term2],
    \+ 0 is Term2,
    0 is Term1 mod Term2.

% When operation is +, - or *
% Used disjunction ; for readability, could've also used member/2
move(Terms, Operation, [Ar | Tmp2]):-
    (Operation = +; Operation = -; Operation = *),
    % member(Operation, [+, -, *]),
    select(Term1, Terms, Tmp),
    select(Term2, Tmp, Tmp2),
    Ar =.. [Operation, Term1, Term2].

% Given Terms, produce a new list by taking to elements of Terms and apply an
% artethmetic operation on those.
move(Target:Terms, Target:NewState):-
    Operations = [+, -, *, /],
    member(X, Operations),
    move(Terms, X, NewState).

% Example of Move:
% ?- move(200:[10, 20], X).
% X = 200:[10+20] ;
% X = 200:[20+10] ;
% X = 200:[10-20] ;
% X = 200:[20-10] ;
% X = 200:[10*20] ;
% X = 200:[20*10] ;
% X = 200:[20/10].

% Solve; computes an arhetmetic combinations of Terms which is equal to Target
solve(Target, Terms, Solution):-
    % solve_iterative_deepening(Target:Terms, Tmp),
    % solve_breadthfirst(Target:Terms, Tmp),
    solve_depthfirst(Target:Terms, Tmp),
    % solve_depthfirst_cyclefree(Target:Terms, Tmp),
    last(Tmp, _:[Solution | _]).

% Example:
% ?- solve(200, [1, 2, 3, 4, 5, 6], X).
% X = (1+3+6)*4*5 .
%
% ?- solve(200, [6, 5, 4, 3, 2, 1], X).
% X = (3+1)* (6+4)*5 ;
%
% ?- solve(10, [6, 5, 4, 3, 2, 1], X).
% X = 6+4 ;
% X = 4+6 ;
% X = 5*2 ;
% X = 2*5 .

% When using the breadthfirst, the algorithm starts at the root, which will be
% the first element of the output-list, and checks for its children if the
% arithmetic value evaluates Goal. So when all children are checked, it finishes
% and therefore it won't be stuck in a loop, in unlike iterative deepening
%
% They both work, and the cycle doesnt make a difference because it cant get
% stuck in a loop.
%
% Because depth-first goes in to depth, which means it first tries to reach to
% the last element of the graph, which will be the last element in the list.
% Before it can reach the last element in the list it has to travel through the
% other elements and takes it into the artethmetic expression.


% Search algorithms, implemented by Ulle Endriss

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logisch Programmeren en Zoektechnieken                    %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Uninformed Search Algorithms                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This file provides all the code for the uniformed search
% algorithms introduced in the course. These are:
%  * simple depth-first search
%  * cycle-free depth-first search
%  * depth-bounded depth-first search
%  * breadth-first search
%  * iterative deepening

% Problem specification:
% All algorithms assume that problems are specified by
% implementing the predicates move/2 and goal/1. The former
% specifies the range of legal follow-up states for any given
% state, while the latter succeeds iff the argument given is
% a valid goal state. See files blocks.pl and eightqueens.pl
% for examples. The initial state, which also forms part of
% the problem specification, has to be given as an argument
% to the search algorithm in question.

% The following is an auxiliary predicate for several of the
% algorithms. It serves as a wrapper around move/2, but will
% fail in case of a cycle. The first argument should be
% instantiated with a list of nodes visited already.

move_cyclefree(Visited, Node, NextNode) :-
  move(Node, NextNode),
  \+ member(NextNode, Visited).

% The actual algorithms follow. Comments have been omitted.
% For explanations on how they work, please refer to the
% lecture slides. In all cases, use the predicate of the
% form solve_* to run the algorithm. The first argument
% should be the initial state; then the variable given as
% the second argument will be instantiated with a solution
% path (in the case of depth-bounded depth-first search,
% these are the second and the third argument respectively;
% the first argument is the bound to be supplied by the user).

% Simple depth-first search:

solve_depthfirst(Node, [Node|Path]) :-
  depthfirst(Node, Path).

depthfirst(Node, []) :-
  goal(Node).

depthfirst(Node, [NextNode|Path]) :-
  move(Node, NextNode),
  depthfirst(NextNode, Path).

% Cycle-free depth-first search:

solve_depthfirst_cyclefree(Node, Path) :-
  depthfirst_cyclefree([Node], Node, RevPath),
  reverse(RevPath, Path).

depthfirst_cyclefree(Visited, Node, Visited) :-
  goal(Node).

depthfirst_cyclefree(Visited, Node, Path) :-
  move_cyclefree(Visited, Node, NextNode),
  depthfirst_cyclefree([NextNode|Visited], NextNode, Path).

% Depth-bounded depth-first search:

solve_depthfirst_bound(Bound, Node, Path) :-
  depthfirst_bound(Bound, [Node], Node, RevPath),
  reverse(RevPath, Path).

depthfirst_bound(_, Visited, Node, Visited) :-
  goal(Node).

depthfirst_bound(Bound, Visited, Node, Path) :-
  Bound > 0,
  move_cyclefree(Visited, Node, NextNode),
  NewBound is Bound - 1,
  depthfirst_bound(NewBound, [NextNode|Visited], NextNode, Path).

% Breadth-first search:

solve_breadthfirst(Node, Path) :-
  breadthfirst([[Node]], RevPath),
  reverse(RevPath, Path).

breadthfirst([[Node|Path]|_], [Node|Path]) :-
  goal(Node).

breadthfirst([Path|Paths], SolutionPath) :-
  expand_breadthfirst(Path, ExpPaths),
  append(Paths, ExpPaths, NewPaths),
  breadthfirst(NewPaths, SolutionPath).

expand_breadthfirst([Node|Path], ExpPaths) :-
  findall([NewNode,Node|Path], move_cyclefree(Path,Node,NewNode), ExpPaths).

% Iterative deepening:

solve_iterative_deepening(Node, Path) :-
  path(Node, GoalNode, RevPath),
  goal(GoalNode),
  reverse(RevPath, Path).

path(Node, Node, [Node]).

path(FirstNode, LastNode, [LastNode|Path]) :-
  path(FirstNode, PenultimateNode, Path),
  move_cyclefree(Path, PenultimateNode, LastNode).
