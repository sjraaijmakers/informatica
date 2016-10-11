% Prolog Homework #4
% 11 oktober 2016
% Steven Raaijmakers, 10804242, sjraaijmakers@gmail.com

% SORTING ALGORITHMS, implemented by Ulle Endriss:

check(Rel, A, B) :-
    step_counter(), % added counter
    Goal =.. [Rel, A, B],
    call(Goal).

% Bubblesort
swap(Rel, [A,B|List], [B,A|List]) :-
    check(Rel, B, A).

swap(Rel, [A|List], [A|NewList]) :-
    swap(Rel, List, NewList).

bubblesort(Rel, List, SortedList) :-
    swap(Rel, List, NewList), !,
    bubblesort(Rel, NewList, SortedList).

bubblesort(_, SortedList, SortedList).

% Improved Bubblesort
bubblesort2(Rel, List, SortedList) :-
    swap2(Rel, List, NewList), % this now always succeeds
    List \= NewList, !, % check there’s been a swap
    bubblesort2(Rel, NewList, SortedList).

bubblesort2(_, SortedList, SortedList).

swap2(Rel, [A,B|List], [B|NewList]) :-
    check(Rel, B, A),
    swap2(Rel, [A|List], NewList). % continue!

swap2(Rel, [A|List], [A|NewList]) :-
    swap2(Rel, List, NewList).

swap2(_, [], []). % new base case: reached end of list

% Quicksort
quicksort(_, [], []).

quicksort(Rel, [Head|Tail], SortedList) :-
    split(Rel, Head, Tail, Left, Right),
    quicksort(Rel, Left, SortedLeft),
    quicksort(Rel, Right, SortedRight),
    append(SortedLeft, [Head|SortedRight], SortedList), !. % cut?!?!?

split(_, _, [], [], []).

split(Rel, Middle, [Head|Tail], [Head|Left], Right) :-
    check(Rel, Head, Middle), !,
    split(Rel, Middle, Tail, Left, Right).

split(Rel, Middle, [Head|Tail], Left, [Head|Right]) :-
    split(Rel, Middle, Tail, Left, Right).

% Homework Assignments:

% Question 1
% A:

% Counter

% Tell prolog counter() will be dynamic
:- dynamic counter/1.

% Initialize Counter, by removing all facts for counter(), and adding counter(0)
init_counter():-
    retractall(counter(_)),
    assert(counter(0)).

% Increase counter by 1. Do this by extracting latest value for counter, and
% incrementing it. Also remove all other facts for counter/0.
step_counter():-
    counter(X),
    NewX is X + 1,
    retractall(counter(_)),
    assert(counter(NewX)).

% Gets current value for counter
get_counter(X):-
    counter(X).

% Example:
% ?- init_counter, get_counter(A), step_counter, step_counter, get_counter(B).
% A = 0,
% B = 2.

% B:
% Experiment returns operations required for computing Algorithm on List.
experiment(Algorithm, List, Count):-
    Term =.. [Algorithm, <, List, _],
    init_counter(),
    call(Term),
    get_counter(Count).

% Example:
% ?- experiment(bubblesort2, [10,3,5,1,2], Count).
% Count = 16.

% C:
% Random list returns a list of random number between 1 and Max of size Size.

% Base case if first two arguments are the same, add empty list
random_list(Count, Count, _, []).

%  Checks wheter Count is lower than Size, then computes a random value between
% 1 and Max, and adds it to Output
random_list(Count, Size, Max, [R | Output]):-
    Count < Size,
    NewCount is Count + 1,
    random_list(NewCount, Size, Max, Output), !,
    random_between(1, Max, R).

% Userfunction: set Count to zero
random_list(Size, Max, Output):-
    random_list(0, Size, Max, Output).

% Example:
% ?- random_list(5, 2, List).
% List = [1, 2, 1, 1, 2].

% D:
% Base case; when only 1 item is left return the corresponding value
my_sumlist([Elem], Elem).

% Computes sum of all items in List by adding the sum of the first two items
% of the list, and call recursive
my_sumlist([Elem1, Elem2 | Tail], Out) :-
    my_sumlist([Elem1 + Elem2 | Tail], Out).

% Random experiment checks how many operations are required to apply Algorithm
% to a random list of length Length.
random_experiment(Algorithm, Length, MaxElem, Count):-
    random_list(Length, MaxElem, Out),
    experiment(Algorithm, Out, Count).

% Random Experiments (plural):
% Base case, whenever count has reached N.
random_experiments(_, _, _, Count, [], Count).

% For each Count < N, do random experiment, and return operations required. This
% is counted via a (new)counter which takes the increment as its parameter.
random_experiments(Algorithm, Length, MaxElem, N, [Tmp | List], Count):-
    Count < N,
    random_experiment(Algorithm, Length, MaxElem, Tmp),
    NewCount is Count + 1,
    random_experiments(Algorithm, Length, MaxElem, N, List, NewCount).

% Userfunction: random experiments runs N experiments on a list with random
% integers between 1 and N, of length Length. It returns the average count.
random_experiments(Algorithm, Length, MaxElem, N, AvgCount):-
    random_experiments(Algorithm, Length, MaxElem, N, Averages, 0), !,
    my_sumlist(Averages, Tmp),
    AvgCount is round(Tmp / N).

% Example:
%
% ?- time(random_experiments(bubblesort, 100, 500, 100, AvgCount)).
% % 88,624,158 inferences, 15.525 CPU in 15.531 seconds (100% CPU, 5708637 Lips)
% AvgCount = 110382 .
%
% Because we took N=100 experiments, the average time will be 15.531 / 100 =
% 0.11531 sec
%
% ?- time(random_experiments(bubblesort2, 100, 500, 100, AvgCount)).
% % 7,251,222 inferences, 1.359 CPU in 1.359 seconds (100% CPU, 5337202 Lips)
% AvgCount = 8930 .
%
% Average time: 1.359 / 100 = 0.1359 sec
%
% ?- time(random_experiments(quicksort, 100, 500, 100, AvgCount)).
% % 660,941 inferences, 0.117 CPU in 0.118 seconds (99% CPU, 5654318 Lips)
% AvgCount = 646 .
%
% Average time: 0.118 / 100 = 0.00118 sec

% E:

% Line: given an integer N return that much itterations of character C

% Base case, when count has reached 0, print new line.
line(0, _):-
    nl, !.

line(N, C):-
    write(C),
    T is N - 1,
    line(T, C).

% Chart: visualizes a chart by taking all the lists with length 1 to length N.
align(Length):-
    Length < 10,
    write(" "), write(Length).

align(Length):-
    write(Length).

% Basecase is when Length is bigger than MaxLength
chart(_, Length, MaxLength, _, _):-
    Length > MaxLength.

% Run random_experiments, and display the outcome as a bar.
chart(Algorithm, Length, MaxLength, MaxElem, N):-
    random_experiments(Algorithm, Length, MaxElem, N, AvgCount),
    N2 is round(AvgCount / 5),
    align(Length), write(" > "), line(N2, "*"),
    NewLength is Length + 1,
    chart(Algorithm, NewLength, MaxLength, MaxElem, N).

% Userfunction: call chart with a countvar included
chart(Algorithm, MaxLength, MaxElem, N):-
    chart(Algorithm, 1, MaxLength, MaxElem, N), !.

% Example:
% ?- chart(bubblesort, 5, 50, 100).
%  1 >
%  2 >
%  3 > *
%  4 > **
%  5 > ***

% Question 2:

% Array representation of our grid:
grid([ [w, w, w, b, w],
       [b ,b, w, w, w],
       [w, w, w, b, w],
       [w, b, b, b, b],
       [w, w, w, w, w] ]).

% Returns true if X/Y is in bottomright-corner of above grid.
goal(X/Y):-
    grid([H | _]),
    length(H, Length),
    X is Length,
    Y is Length.

% Examples:
% ?- goal(5/5).
% Yes
%
% ?- goal(4/5).
% No

% Base case; when N is 1, return head of list
my_nth(1, [X|_], X):- !.

% Gets Nth item of List by removing head an decreasing N
my_nth(N, [_ | List], X) :-
    N > 1,
    NewN is N - 1,
    my_nth(NewN, List, X).

% Checks wheter X/Y of grid is white (w)
white(X/Y):-
    grid(Grid),
    nth1(Y, Grid, Row),
    nth1(X, Row, w).

% Defines all legal moves from X/Y
move(X/Y, NewX/NewY):-
    between(-1, 1, TmpX),
    between(-1, 1, TmpY),
    0 is TmpX * TmpY, % don't move diagonally
    NewX is X + TmpX,
    NewY is Y + TmpY,
    \+ NewX/NewY = X/Y, % actually move
    white(NewX/NewY). % check if new position is w

% Examples:
% ?- move(5/5, NextState).
% NextState = 4/5 ;
% false.

% Depth first v1, implemented by Ulle Endriss
solve_depthfirst(Node, [Node|Path]) :-
    depthfirst(Node, Path).

depthfirst(Node, []) :-
    goal(Node).

depthfirst(Node, [NextNode|Path]) :-
    move(Node, NextNode),
    depthfirst(NextNode, Path).

% Depth first v2, implemented by Ulle Endriss
move_cyclefree(Visited, Node, NextNode) :-
    move(Node, NextNode),
    \+ member(NextNode, Visited).

solve_depthfirst_cyclefree(Node, Path) :-
    depthfirst_cyclefree([Node], Node, RevPath),
    reverse(RevPath, Path).

depthfirst_cyclefree(Visited, Node, Visited) :-
    goal(Node).

depthfirst_cyclefree(Visited, Node, Path) :-
    move_cyclefree(Visited, Node, NextNode),
    depthfirst_cyclefree([NextNode|Visited], NextNode, Path).

% Depth first v3, implemented by Ulle Endriss
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

% C:
% Depth first v1
% ?- solve_depthfirst(1/1, Plan).
% ERROR: Out of local stack
%
% The first version is stuck, because when the node has changed to 3/2 it
% moves back to 3/1, and when it's in 3/1 it will move back to 3/2 and so on.
% It's stuck in an endless loop

% Depth first cyclefree
% ?- solve_depthfirst_cyclefree(1/1, Plan).
% Plan = [1/1, 2/1, 3/1, 3/2, 3/3, 2/3, 1/3, 1/4, ... / ...|...] ;
% false.
%
% This one does work because it has cyclefree build-in, so it when it's in a
% loop it moves to another option.

% Depth first Bound
% ?- solve_depthfirst_bound(11, 1/1, Plan).
% false.
%
% ?- solve_depthfirst_bound(12, 1/1, Plan).
% Plan = [1/1, 2/1, 3/1, 3/2, 3/3, 2/3, 1/3, 1/4, ... / ...|...] ;
% false.
%
% This one does work for bound > 11 because it takes the algorithm 12 steps to
% reach the goal. So if the bound is set to 11 it returns false because it cant
% finish in 11 steps.

% D:
% DRAW FUNCTIONS: only looks like an animation on my computerscreen!!!!!!

% When end of row is reached, print border and nl.
row(N, X, _, _, _):-
    X > N, write("|"), nl.

% When X/Y is the searched element (H of Grid should be w also)
row(N, X, Y, Elem, [_ | GridRow]):-
    Elem = X/Y,
    write("<>"),
    NewX is X + 1,
    row(N, NewX, Y, Elem, GridRow).

% When X/Y is W in Grid
row(N, X, Y, Elem, [H | GridRow]):-
    H = w,
    write("  "),
    NewX is X + 1,
    row(N, NewX, Y, Elem, GridRow).

% When X/Y is B in Grid
row(N, X, Y, Elem, [_ | GridRow]):-
    write("XX"),
    NewX is X + 1,
    row(N, NewX, Y, Elem, GridRow).

% When Y counter has reached N; square is finished.
% Print newlines to "simulate" animation
square(N, _, Y, _, _):-
    Y > N,
    write(" "), line(N, "--"), line(38, "\n").

% Draw square with the counters (X/Y) included.
% Last parameter is the Yth row of Grid
square(N, X, Y, Elem, [H | T]):-
    write("|"),
    row(N, X, Y, Elem, H),
    NewY is Y + 1,
    square(N, X, NewY, Elem, T).

% Draw square NxN with Elem marked in Grid. Start counting at 1/1
square(N, Elem, Grid):-
    write(" "), line(N, "--"), % draws top border
    % Call with 2 counters: x/y.
    square(N, 1, 1, Elem, Grid), !.

% When all elements have been drawn
animate([], _).

% Animate: for each element in List draw in grid
animate([H | T], Grid):-
    length(Grid, Length), % assuming its a square
    square(Length, H, Grid),
    sleep(0.3), % sleep so the humaneye sees an animation
    animate(T, Grid).

% User function:
% Use grid to animate over
animate(List):-
    grid(Grid),
    animate(List, Grid).

% Representation of grid at init:
%  ----------
% |<>    XX  |
% |XXXX      |
% |      XX  |
% |  XXXXXXXX|
% |          |
%  ----------

% Example:
% ?- solve_depthfirst_cyclefree(1/1, Plan), animate(Plan).
% This will print the animation for the solution in Plan..
