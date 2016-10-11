% Prolog Homework #3
% 27 september 2016
% Steven Raaijmakers, 10804242, sjraaijmakers@gmail.com

% Some arithmetic operations which can be applied to a Number
positive(Number) :- Number > 0.
even(Number) :- Number mod 2 =:= 0.
square(Number) :- Number >= 0, sqrt(Number) =:= round(sqrt(Number)).

% Extract: Takes first argument, translates it to a function which it will
% apply to each element of the List

% Base case; when lists are empty
extract(_, [], []).

% Takes head of list, applys the function to it and when its true; append to
% output
extract(Arithmetic, [H | T], [H | Output]):-
	X =.. [Arithmetic, H], % From atoms to function
	X, !, % make sure the function returned True
	extract(Arithmetic, T, Output). % recursive call, without the head

% When "X is false"; next item
extract(Arithmetic, [_ | T], Output):-
	extract(Arithmetic, T, Output).

% Some facts
countries([belgium, france, germany, italy, luxembourg, netherlands]).
weight(france, 4).
weight(germany, 4).
weight(italy, 4).
weight(belgium, 2).
weight(netherlands, 2).
weight(luxembourg, 1).
threshold(12).

% Winning function: checks wheter weight of Countries >= treshold

% Base case, when empty-list is given; return 0.
winning([], 0).

% Get Head of List
winning([H | T], NewOutput):-
	weight(H, Tmp), % Save weight of H in Tmp
	winning(T, Output), % Recursive call without the head
	NewOutput is Tmp + Output. % Add weight of Tmp to Output

% User function
winning(List):-
	winning(List, X), % Put score of winning in X
	threshold(Threshold),
	X >= Threshold. % Check if X is more or equal to Treshold

% Critical: checks which country is "critical" to certain Countries
critical(Country, Countries):-
	\+ member(Country, Countries),
	\+ winning(Countries),
	winning([Country | Countries]).

% Sublist returns an sublist of List in Output (via backtracking)

% Base case; empty list only has an empty list as sublist
sublist([], []).

% add head to output an recursive call on tail
sublist([H | T], [H | Output]):-
    sublist(T, Output).
% remove head from output via recursive call
sublist(E, [_ | Output]) :-
    sublist(E, Output).

% Voting power uses findall/3 to find all Coalitions which a Country is
% critical to. By using findall we put all the different Coalitions in one
% list. Thereafter we count the length of this list and output it as Power
voting_power(Country, Length):-
	findall(Coalition, (countries(All), sublist(Coalition, All), critical(Country, Coalition)), List),
	length(List, Length).

% Divisors: find all natural numbers we can divide N by
% I choose to start by dividing N by N-1 instead of 1, so I didn't have to
% check wheter the Counter is still below N, or equal to (seems cleaner)

% Basecase, when C reached 0 return empty list
divisors(_, 0, []).

% If N is divisble by C, append X to Output List
divisors(N, C, [C | X]):-
	N / C =:= round(N / C), % integer check
	NewC is C - 1,
	divisors(N, NewC, X). % Check next C (by C + 1)

% If N is not divisble, don't append to output, but increase counter
divisors(N, C, X):-
	NewC is C - 1,
	divisors(N, NewC, X).

% Check wheter N is divisble by [n, n-1, ... 1]. Userfunction:
divisors(N, [N | X]):-
	divisors(N, N - 1, X), !.
