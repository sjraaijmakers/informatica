% Prolog Homework #5.2
% 18 oktober 2016
% Steven Raaijmakers, 10804242, sjraaijmakers@gmail.com

% Question 2:

% Roads between cities (disclaimer; they don't make sense)
distance(amsterdam, utrecht, 40).
distance(amsterdam, haarlem, 30).
distance(haarlem, alkmaar, 40).
distance(alkmaar, zaandam, 30).
distance(hertogenbosch, breda, 30).
distance(eindhoven, breda, 20).
distance(rotterdam, breda, 50).
distance(utrecht, breda, 70).
distance(groningen, alkmaar, 50).
distance(utrecht, alkmaar, 80).
distance(amsterdam, zwolle, 110).
distance(utrecht, zwolle, 90).
distance(groningen, zwolle, 105).

% Cities and their coordinates
city(hertogenbosch, 60, 0).
city(breda, 80, 0).
city(eindhoven, 80, 10).
city(rotterdam, 70, 50).
city(utrecht, 108, 70).
city(amsterdam, 110, 100).
city(haarlem, 85, 96).
city(alkmaar, 80, 120).
city(groningen, 200, 190).
city(zwolle, 160, 110).

% Node is of form: Begin/End:Path where Begin and End are cities and Path
% a list of cities between Begin and End.

% Goal is reached when last element of Path equals End.
goal(End:Path):-
    last(Path, End).

% Move function determines a new state based on the connections of the last
% element of Path.
move(End:Path, End:Result, Cost):-
    last(Path, Tmp),
    % When A is connected to B, B is connected to A (improves readability)
    (distance(Tmp, NextState, Cost) ; distance(NextState, Tmp, Cost)),
    \+ member(NextState, Path), % no loops
    append(Path, [NextState], Result).

% Compute distance between City1 and City2 based on their x,y-citys
get_distance(X1, Y1, X2, Y2, Distance):-
    Distance is sqrt(abs(X1 - X2)^2 + abs(Y1 - Y2)^2).

% Get line distance
estimate(End:Path, Estimate):-
    last(Path, Current),
    city(Current, X1, Y1),
    city(End, X2, Y2),
    get_distance(X1, Y1, X2, Y2, Estimate).

% A* algorithm: implemented by Ulle Endriss
get_best([Path], Path) :- !.
get_best([Path1/Cost1/Est1,_/Cost2/Est2|Paths], BestPath) :-
    Cost1 + Est1 =< Cost2 + Est2, !,
    get_best([Path1/Cost1/Est1|Paths], BestPath).
get_best([_|Paths], BestPath) :-
    get_best(Paths, BestPath).

move_astar([Node|Path]/Cost/_, [NextNode,Node|Path]/NewCost/Est) :-
    move(Node, NextNode, StepCost),
    \+ member(NextNode, Path),
    NewCost is Cost + StepCost,
    estimate(NextNode, Est).

expand_astar(Path, ExpPaths) :-
    findall(NewPath, move_astar(Path,NewPath), ExpPaths).

astar(Paths, Path) :-
    get_best(Paths, Path),
    Path = [Node|_]/_/_,
    goal(Node).

astar(Paths, SolutionPath) :-
    get_best(Paths, BestPath),
    select(BestPath, Paths, OtherPaths),
    expand_astar(BestPath, ExpPaths),
    append(OtherPaths, ExpPaths, NewPaths),
    astar(NewPaths, SolutionPath).

solve_astar(Node, Path/Cost) :-
    estimate(Node, Estimate),
    astar([[Node]/0/Estimate], RevPath/Cost/_),
    reverse(RevPath, Path).

% Userfunction:
route(Begin, End, Path, Cost):-
    solve_astar(End:[Begin], Tmp/Cost),
    last(Tmp, _:Path).

% Examples of route/4:
% ?- route(breda, haarlem, Path, Cost).
% Path = [breda, utrecht, amsterdam, haarlem],
% Cost = 140 ;
% Path = [breda, utrecht, zeist, amsterdam, haarlem],
% Cost = 170 ;
% Path = [breda, utrecht, alkmaar, haarlem],
% Cost = 190 .
%
% ?- route(amsterdam, groningen, Path, Cost).
% Path = [amsterdam, haarlem, alkmaar, groningen],
% Cost = 120 ;
% Path = [amsterdam, zwolle, groningen],
% Cost = 215 ;
% Path = [amsterdam, utrecht, zwolle, groningen],
% Cost = 235.

% Question 3:
% ???
