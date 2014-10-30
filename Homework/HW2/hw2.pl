%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logisch Programmeren en Zoektechnieken 2014               %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Homework #2: Answers                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The operator definitions from the exercise:

:- op(100, yfx, plink),
   op(200, xfy, plonk).

% (a) (i)
% ?- tiger plink dog plink fish = X plink Y.
% X = tiger plink dog
% Y = fish
% Yes
% Explanation: Because plink is left-associative, X is
% matched to the complex sub-term (rather than to tiger).

% (a) (ii)
% ?- cow plonk elephant plink bird = X plink Y.
% No
% Explanation: The query fails, because the principal
% operator of the term on the left is plonk (because
% it has a higher precedence value than plink), whereas
% the principal operator of the term on the right is
% plink. Hence, matching is not possible.

% (a) (iii)
% ?- X = (lion plink tiger) plonk (horse plink donkey).
% X = lion plink tiger plonk horse plink donkey
% Yes
% Explanation: The brackets disappear, because they are
% redundant (plonk has a higher precedence value than plink,
% i.e., it will be the principal functor even without the
% brackets indicting the structure of the term in the query).

% (b)
% Simply try to match the argument with a plink-term and a
% plonk-term, respectively. The rest is just putting out
% the results.

pp_analyse(Left plink Right) :-
  write('Principal operator: plink'), nl,
  write('Left sub-term: '),
  write(Left), nl,
  write('Right sub-term: '),
  write(Right).

pp_analyse(Left plonk Right) :-
  write('Principal operator: plonk'), nl,
  write('Left sub-term: '),
  write(Left), nl,
  write('Right sub-term: '),
  write(Right).

% Run pp_analyse/1 on some example terms, like those from
% question (a) to get a better idea of what operator
% precedence and associativity really mean.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Here are again the operator definitions from the exercise:

:- op(100, fx, the),
   op(100, fx, a),
   op(200, xfx, has).

% (a) Indicate the structure of this term using parentheses
% and name its principal functor: claudia has a car

% Structure: (claudia) has (a (car))
% You can verify this by checking that Prolog does indeed
% treat all of these parentheses as being redundant (i.e., 
% they will not be shown in the answer):
% ?- X = (claudia) has (a (car)).
% X = claudia has a car
% Yes

% Principal functor: has
% You can verify this using the built-in predicate functor/3:
% ?- functor(claudia has a car, Functor, Arity).
% Functor = has
% Arity = 2
% Yes

% (b) What would Prolog reply when presented with the
% following query?
% ?- the lion has hunger = Who has What.
% Who = the lion
% What = hunger
% Yes
% Explanation: The term on the left and the term on the
% right are being matched. The principal functor in both
% cases is "has", i.e., Prolog will match the term (the lion)
% with the variable Who, and the atom hunger with the variable
% What.

% (c) Explain why the following query would cause a syntax error:
% ?- X = she has whatever has style.
% ERROR: Syntax error: Operator priority clash
% ERROR: X = she has
% ERROR: ** here **
% ERROR:  whatever has style .
% Explanation: The operator "has" is defined as a non-associative
% infix operator (type xfx). That means that both sub-terms of
% any term with "has" as the principal operator would have to
% have a precedence value that is strictly less than 200. Hence,
% whichever of the two occurrences of "has" in the above term 
% we take to be the principal operator, we end up with a
% contradiction, because one of the two sub-terms would itself
% have "has" as its principal operator and thereby have a
% precedence value of 200.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 3                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We start by implementing a number of auxiliary predicates.
% The following list of facts indicates for each of the four
% orientations how moving in that direction impacts on the
% current position.

direction(north, 0, 1).
direction(south, 0, -1).
direction(west, -1, 0).
direction(east, 1, 0).

% The next set of facts provide for each given orientation
% the new direction we obtain if we turn right.

right(north, east).
right(east, south).
right(south, west).
right(west, north).

% The following rule provides the same functionality for
% turning left.

left(Ori1, Ori2) :- right(Ori2, Ori1).

% Now we can give a simple and short implementation of execute/5.
% First, moving forward does not affect the current orientation,
% while we can obtain the new position by adding the values
% provided by direction/3 to the current position. For example,
% if the current orientation is north, then we have to add 0 to
% the X-value and 1 to the Y-value to obtain the new position.

execute((X,Y), Ori, move, (NewX,NewY), Ori) :-
  direction(Ori, DirX, DirY),
  NewX is X + DirX,
  NewY is Y + DirY.

% Turning right (or left) does not affect the current position.
% We obtain the new orientation by querying right/2 (or left/2).

execute(Pos, Ori, right, Pos, NewOri) :-
  right(Ori, NewOri).

execute(Pos, Ori, left, Pos, NewOri) :-
  left(Ori, NewOri).

% The above solution for execute/5 is nice in so far as we
% only have to give a single rule for each of the three commands.
% An alternative solution (shown below), without auxiliary
% predicates, would implement a separate set of three rules for
% each of the four directions.

% execute((X,Y), north, move, (X,NewY), north) :- NewY is Y + 1.
% execute(Pos, north, right, Pos, east).
% execute(Pos, north, left, Pos, west).
%
% execute((X,Y), south, move, (X,NewY), south) :- NewY is Y - 1.
% execute(Pos, south, right, Pos, west).
% execute(Pos, south, left, Pos, east).
%
% execute((X,Y), west, move, (NewX,Y), west) :- NewX is X - 1.
% execute(Pos, west, right, Pos, north).
% execute(Pos, west, left, Pos, south).
%
% execute((X,Y), east, move, (NewX,Y), east) :- NewX is X + 1.
% execute(Pos, east, right, Pos, south).
% execute(Pos, east, left, Pos, north).

% The predicate status/5 is similar to execute/5, but takes a list of 
% commands rather than a single command as the middle argument. When that 
% list is empty, the output position and orientation are equal to the 
% input position and orientation (base case). For the recursive rule, we 
% call execute/5 with the command in the head of the list of commands and 
% then continue recursively with the tail of the list.

status(Pos, Ori, [], Pos, Ori).

status(Pos, Ori, [Com|Coms], FinalPos, FinalOri) :-
  execute(Pos, Ori, Com, NewPos, NewOri),
  status(NewPos, NewOri, Coms, FinalPos, FinalOri).

% Finally, status/3 calls status/5 with the initial position set to (0,0) 
% and the initial orientation set to north.

status(Coms, Pos, Ori) :-
  status((0,0), north, Coms, Pos, Ori).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 5                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We first implement a predicate prime/1 that will succeed if
% the argument provided is a prime number.

% We start by writing an auxiliary predicate prime/2 that will
% succeed if, when given arguments K and N, it is the case that
% neither K nor any of the numbers (strictly) between K and N
% divide N. We will give a recursive solution, where K is
% increased by 1 in every step.

% When can we be sure that K is so large that definitely no
% larger number could possibly still divide N? The earliest
% we can be sure about this is when K is larger than the square
% root of N, as any (proper) divisor on N must be equal to its
% square root or less. So we can use this as the stopping
% condition for our base case. Of course, other (less efficient)
% stopping conditions (such as K >= N) are also possible.

prime(K, N) :-
  K > sqrt(N).

% For the recursive rule, we increment K in every step, and
% call the same predicate again with the new value. Before doing
% so we check that the current K indeed does not divide N, i.e.,
% we check whether the rest of the integer division of N by K
% leaves a rest that is different from 0. 

prime(K, N) :-
  K =< sqrt(N),
  N mod K =\= 0,
  NewK is K + 1,
  prime(NewK, N).

% Now we can implement prime/1 by simply checking, for the given
% number N, that none of the numbers between 2 (inclusive) and N
% (exclusive) divide N. We include the condition N > 1 to ensure
% the predicate fails for 1 and smaller numbers.

prime(N) :-
  N > 1,
  prime(2, N).

% To test the Goldbach Conjecture for a given even integer N, we
% need to select a number lower than N, check that it is prime,
% and finally check that the remainder is also prime. In fact,
% for the first number it is sufficient to restrict attention to
% numbers between 2 (the smallest prime number) and N/2 (as the
% smaller of the two primes adding up to N surely cannot be
% greater than N/2).

goldbach(N, Prime1 + Prime2) :-
  Half is N // 2,
  between(2, Half, Prime1),
  prime(Prime1),
  Prime2 is N - Prime1,
  prime(Prime2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 5                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load the list of words:

:- consult(words).

% Converting a word represented by an atom into a list of
% letters is exactly the same operation as decomposing an
% atom into characters:

word_letters(Word, Letters) :-
  atom_chars(Word, Letters).

% To check whether one list is covered by a second list, we
% use a recursive approach. First, the empty list is covered
% by any other list (base case). For the recursive rule, we
% first check whether the head of the first list can be found
% in the second list. If so, we use select/3 to return the
% remainder of the second list after removing one instance of
% the head of the first. We then recursively check whether
% the tail of the first list is covered by that remainder.

cover([], _).

cover([Head|Tail], List) :-
  select(Head, List, Remainder),
  cover(Tail, Remainder).

% To search for a word of a given length covered by a given
% list of letters, we first retrieve a word, then convert it
% into a list of letters, check whether that list is covered
% by the input list, and check that that same list has the
% required length. To actually find such a word, Prolog will
% simply try again and again retrieving new words from the
% list of facts, until one meeting our requirements is found.

solution(List, Word, Score) :-
  word(Word),
  word_letters(Word, Letters),
  cover(Letters, List),
  length(Letters, Score).

% A top solution is a solution maximising the score. Thus, now we are
% searching for the longest possible word covered by the given list of 
% letters. We start looking for words of the maximally possible length, 
% which is equal to the number of letters in the input list, and keep 
% decrementing the score we try to achieve until we have found a word
% constituting a solution.

% We first implement a predicate topsolution/4, taking as input arguments 
% the list of letters in the first position and a score to try next in the 
% third position. It will return the word found in the second position and 
% the actual score in the fourth position. The base case is the case where
% a solution of the score currently tried can be found (which we can check
% using solution/3). In that case, the actual score to be returned is equal
% to the current trial score. For the recursive rule, we first check that
% it is still possible to further reduce the score currently tried (i.e.,
% whether it is still greater than 1). If so, we decrement it by 1 and 
% continue with a recursive call for this new trial score.

topsolution(List, Word, Score, Score) :-
  solution(List, Word, Score).

topsolution(List, Word, TryScore, ActualScore) :-
  TryScore > 1,
  NewTryScore is TryScore - 1,
  topsolution(List, Word, NewTryScore, ActualScore).

% The predicate topsolution/3 takes a list of letters and returns a solution 
% word with maximal score. It simply initialises topsolution/4 with the highest 
% possible value for the score to be tried, which is equal to the length of the 
% input list of letters.

topsolution(List, Word, Score) :-
  length(List, IdealScore),
  topsolution(List, Word, IdealScore, Score).

% Below are three examples for topsolution/3 in action.

% Example taken from the Wikipedia article on Countdown:
% ?- topsolution([g,y,h,d,n,o,e,u,r], Word, Score).
% Word = greyhound
% Score = 9
% Yes

% From Countdown, Channel 4, 10 September 2014:
% ?- topsolution([s,e,k,w,u,t,d,e,l], Word, Score).
% Word = lewdest,
% Score = 7 
% Yes

% From Countdown, Channel 4, 18 December 2002:
% ?- topsolution([y,c,a,l,b,e,o,s,x], Word, Score).
% Word = calyxes,
% Score = 7
% Yes

% Thus, our program has found a better solution than the human champion!


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 6                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% When computing Fibonacci numbers, we have to consider two
% base cases, namely for 0 and for 1. In the recursion step
% we first get the predecessor of N and the predecessor of
% that number. Then we compute the Fibonacci numbers for
% those indices (recursively) and add up the results to get
% the Nth member of the Fibonacci sequence. 

fibonacci(0, 1).

fibonacci(1, 1).

fibonacci(N, F) :-
  N >= 2,
  N1 is N - 1,
  N2 is N - 2,
  fibonacci(N1, F1),
  fibonacci(N2, F2),
  F is F1 + F2.

% The above implementation is a straightforward translation
% from the mathematical definition of the Fibonacci sequence
% into Prolog. It works, but it is not very efficient. For
% instance, it takes Prolog quite a while to reply to a query
% such as fibonacci(23,X). On my machine, for the value 30
% Prolog already runs out of memory (the maximum value Prolog
% can handle may vary slightly from system to system):

% ?- fibonacci(23, X).
% X = 46368
% Yes
% ?- fibonacci(30, X).
% ERROR: Out of local stack

% It is also interesting to time a query such as fibonacci(23,X)
% and to look at the number of inference steps reported by the
% Prolog system:

% ?- time(fibonacci(23,X)).
% 278,203 inferences, 3.00 CPU in 3.00 seconds (100% CPU, 92734 Lips)
% X = 46368
% Yes

% To understand why this is so difficult for Prolog, imagine the
% goal execution tree you would have to draw to analyse a query
% involving fibonacci/2.

% Here is a more efficient implementation. The predicate fastfibo/3,
% computes both the N-1st and the Nth Fibonacci number if given
% a number N in the first argument. The main predicate fastfibo/2
% then simply uses fastfibo/3:

fastfibo(N, F) :-
  fastfibo(N, _, F).

% For the implementation of fastfibo/3, there is only a single base
% case: for N=1, it has to return F_0=1 and F_1=1. For the
% recursion step, we first run fastfibo/3 for N-1. This gives us
% the N-2nd and the N-1st Fibonacci numbers. We then obtain
% the Nth Fibonacci number by adding up those two values.

fastfibo(1, 1, 1).

fastfibo(N, F1, F) :-
  N >= 2,
  N1 is N - 1,
  fastfibo(N1, F2, F1),
  F is F1 + F2.

% In fastfibo/3, we only have one recursive call in the main rule,
% rather than the two recursive calls of fibonacci/2. Imagine what
% this does to the goal execution tree. For fibonacci/2, at every
% node in the tree we have to start two new subtrees. For fastfibo/3,
% on the other hand, we only have to consider one subtree. To get
% a feeling for the (immense) difference this makes, try a few
% more timed queries:

% ?- time(fastfibo(23,X)).
% 90 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
% X = 46368
% Yes

% ?- time(fastfibo(42,X)).
% 166 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
% X = 433494437
% Yes

% That is, for N=23, we now get 90 rather than 278,203 inference
% steps. And we are now able to compute F_42 without problems,
% while we could not even get F_30 to work before.

% Note that fastfibo/2 does not define what the 0th Fibonacci number
% is. To cover that case as well, we could add the fact fastfibo(0,1)
% to our program.

% Another approach would be to use a closed form expression to compute
% the Nth Fibonacci number directly. There is such a closed form expression
% for the Fibonacci sequence which is based on the so-called golden ratio.
% Here we just give the formula without any explanation (look up "Fibonacci"
% on Wikipedia to find out more):

goldfibo(N, X) :-
  Gold is (sqrt(5) + 1) / 2,
  X is round((Gold ** (N+1) - (1 - Gold) ** (N+1)) / sqrt(5)).

% Note that some authors define the 0th Fibonacci number to be 0 rather than
% 1. Of course, this affects also the variant of the closed form expression
% that you are going to find in the literature. Both fastfibo/2 and goldfibo/2
% are surprisingly fast. Here is a sample run for the latter:

% ?- time(goldfibo(42,X)).
% 3 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
% X = 433494437
% Yes

% In fact, goldfibo/2 is even faster than fastfibo/2. However, the arithmetic
% operations involved in goldfibo/2 are more complex than those in fastfibo/2.
% This causes a number of problems. The first is that for very large input
% numbers, goldfibo/2 will crash due to the size of the numbers that need
% to be handled as intermediate results. Example:

% ?- goldfibo(5000, X).
% ERROR: is/2: Arithmetic: evaluation error: `float_overflow'

% Our predicate fastfibo/2 can still handle this case (at least in SWI-Prolog,
% which is particularly strong as far as integer arithmetic is concerned).
% This takes 0.01 seconds on my machine:

% ?- fastfibo(5000, X).
% X = 6276302800488957086035253108349684055478528702736457439025824448927937256811663264475883711527806250329984690249846819800648580083040107584710332687596562185073640422286799239932615797105974710857095487342820351307477141875012176874307156016229965832589137779724973854362777629878229505500260477136108363709090010421536915488632339240756987974122598603591920306874926755600361865354330444681915154695741851960071089944015319300128574107662757054790648152751366475529121877212785489665101733755898580317984402963873738187000120737824193162011399200547424034440836239726275765901190914513013217132050988064832024783370583789324109052449717186857327239783000020791777804503930439875068662687670678802914269784817022567088069496231111407908953313902398529655056082228598715882365779469902465675715699187225655878240668599547496218159297881601061923195562143932693324644219266564617042934227893371179832389642895285401263875342640468017378925921483580111278055044254198382265567395946431803304304326865077742925818757370691726168228648841319231470626 
% Yes

% Another (much more serious) problem is that goldfibo/2 introduces rounding
% errors already for much smaller inputs. We can use the following query
% to find the first N for which fastfibo/2 and goldfibo/2 give different
% answers (between/3 is a built-in predicate that, if used as below, will
% return the integers between 1 and 100 through backtracking):

% ?- between(1, 100, N), fastfibo(N, X), goldfibo(N, Y), X =\= Y.
% N = 70,
% X = 308061521170129,
% Y = 308061521170130
% Yes

% Which is the correct answer? This is easy to check: both predicates give
% the same (correct) answers for the 68th and the 69th Fibonacci numbers,
% and if we add them up we see that only the answer returned by fastfibo/2
% is correct.
