% Prolog Homework #2
% 20 september 2016
% Steven Raaijmakers, 10804242, sjraaijmakers@gmail.com

% Assignment 1:
:- op(100, yfx, plink),
op(200, xfy, plonk).

% A:

% I:
% ?- tiger plink dog plink fish = X plink Y.
% X = tiger plink dog,
% Y = fish.
% This is the output because plink is yfx (left associative) and thus the input
% can be read as:
% plink(plink(tiger, dog), fish) = plink(X, Y)
% X = plink(tiger, dog) = tiger plink dog
% y = fish

% II:
% ?- cow plonk elephant plink bird = X plink Y.
% false
% The input can be rewritten as:
% plonk(cow, plink(elephant, bird)) = plink(x, y)
% Which will terminates "false" because plink and plonk don't match

% III:
% X = (lion plink tiger) plonk (horse plink donkey).
% X = lion plink tiger plonk horse plink donkey.
% The output is the same as the input but without the parenthesis, because
% plink will be evaluated before plonk anyway.

% B:
% Prolog tries to match the parameters against the parameters in this code,
% so when it finds a expression like X plonk Y, it will terminate the first one.

pp_analyse(X plonk Y):-
    write("Principal opperator: plonk"), nl,
    write("Left subterm: "), write(X), nl,
    write("Right subterm: "), write(Y), nl.

pp_analyse(X plink Y):-
    write("Principal opperator: plink"), nl,
    write("Left subterm: "), write(X), nl;
    write("Right subterm: "), write(Y), nl.

% Examples:
% ?- pp_analyse(dog plink cat plink horse).
% Principal operator: plink
% Left subterm: dog plink cat
% Right subterm: horse
%
% ?- pp_analyse(dog plonk cat plonk horse).
% Principal operator: plonk
% Left subterm: dog
% Right subterm: cat plonk horse


% Assignment 2:
:- op(100, fx, the),
op(100, fx, a),
op(200, xfx, has).

% A:
% claudia has a car can be rewritten as:
% has(claudia, a car) == (claudia) has (a car).
% Therefore "has" is the principal functor.

% B:
% ?- the lion has hunger = Who has What.
% Who = the lion,
% What = hunger.
% Because has is the functor, and it tries to assign a value to the variable
% "Who", which will be lion, and "What" which will be hunger.

% C:
% "has" is xfx, which means non-associative, so no nesting is not alllowed.

% Assignment 3 (3.14):
:- consult(words).

% Splits Word's letters into List
word_letters(Word, List):-
    atom_chars(Word, List).

% Examples:
% ?- word_letters("hello", X).
% X = [h, e, l, l, o].
%
% ?- word_letters("test", X).
% X = [t, e, s, t].

% Checks wheter all elements of [H | T] occur in List.
% Select checks if element X is in a list L, and when true it returns the list L
% withouth element X
cover([], _).
cover([H | T], List):-
    select(H, List, Output),
    cover(T, Output).

% Example:
% cover([a, a, b, b],[b, a, b, a]).
% true.

% Checks if there can be made a word of desired length (Score) with given the
% Letters.
solution(Letters, Word, Score):-
    word(Word), % find all words
    word_letters(Word, WL), % get all words length
    length(WL, Score), % check wheter word length match Score
    cover(WL, Letters). % also check if the letters cover the found words

% Example:
% ?- solution([q,e,r,x],Word, 3).
% Word = exr ;
% Word = req ;
% Word = rex ;

% Find solution/3 when Score and N are equal.
topsolution_h(Letters, Word, N, N):-
    solution(Letters, Word, N).

% While N is bigger than 1 (min. word size will be 2?? letters), count down
% to find a solution
topsolution_h(Letters, Word, Score, N):-
    N > 2,
    Tmp is N - 1,
    topsolution_h(Letters, Word, Score, Tmp).

% Topsolution finds longest word possible which can be made of Letters.
% Count given Letters (N), and try to find if there's a solution for Letters
% in combination with [N, N-1, ..., 1]
topsolution(Letters, Word, Score):-
    length(Letters, N),
    % Once the length is found, call help-function
    topsolution_h(Letters, Word, Score, N).

% Examples:
% ?- topsolution([y,c,a,l,b,e,o,s,x], Word, Score).
% Word = calyxes,
% Score = 7 ;
% Word = casebox,
% Score = 7.
%
% % ?- topsolution([n,y,q,c,u,o,e,t,u], Word, Score).
% Word = unquote,
% Score = 7.
%
% ?- topsolution([i,t,e,x,g,e,q,a,t], Word, Score).
% Word = taxite,
% Score = 6.

% Assignment 4 (3.13):

% Prime Number:
% Given a number N, we'll check if it's a primenumber, by checking if N is not
% divisble by any number between 2 and N - 1.

% prime_h checks wheter X is divisble by Y
prime_h(X, Y) :-
    0 =:= X mod Y.

% Find if X is divisble by [Y, Y-1, ... 2]
prime_h(X, Y) :-
    Y > 2,
    Tmp is Y - 1,
    prime_h(X, Tmp).

% 2 and 3 are primes
prime(2).
prime(3).
% all other cases:
prime(X) :-
    Tmp is X - 1,
    \+ prime_h(X, Tmp). % not?!

% Examples:
% prime(2).
% true .
% prime(2).
% true .
% prime(11).
% true.

% Goldbach:
% Finds two prime numbers where the sum will be Num. This is done by taking
% A by taking half of N and find a number between 2 and Half. B will be N - A.
% Next there will be checked if sthey're both primes.
goldbach(Num, A + B):-
    Half is round(Num / 2),
    between(3, Half, A),
    B is Num - A,
    prime(A),
    prime(B).

% TODO: FIX wrong input (INT) ?

% Examples:
% ?- goldbach(3838, X).
% X = 5+3833 ;
% X = 17+3821.

% Assignment 5 (3.4):

% Slow fibonacci:
% This produces the Nth fibonacci number, by taking the sum of the N-2th and the
% N-1th fibonacci number.
fibonacci(0, X):-
    X is 1.
fibonacci(1, X):-
    X is 1.
fibonacci(N, X):-
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, X1),
    fibonacci(N2, X2),
    X is X1 + X2.

% This is slow because when taking the Nth fibonacci number, we first have to
% compute all the fibonacci from 0 to N (the recursive way):
% fibonacci(4) -> fibonacci(3) + fibonacci(2).
% fibonacci(3) -> fibonacci(2) + fibonacci(1).
% fibonacci(2) -> fibonacci(1) + fibonacci(0).
% So e.g. when we take the 4th fibonacci number, the 2th fibonacci will be
% computed twice, which will be slow ofcourse
% TODO:!?!?!
