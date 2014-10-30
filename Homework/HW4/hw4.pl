%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logisch Programmeren en Zoektechnieken 2014               %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Homework #4: Answers                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% While you were expected to submit a complete solution in a
% single file, I'm providing all sorting and (uniformed)
% search algorithms in separate files (downloadable from the
% Course Information folder on Blackboard). The reason is
% that I want you to be able to use these general
% implementations also later on; they are not specific to
% this homework.

:- consult('sort.pl'),
   consult('search.pl').

% Note that consulting sort.pl this way will cause a warning, 
% because the implementation of check/3 in sort.pl is being
% overridden (see Question 1b). This is not an ideal solution,
% but it means that you can also use sort.pl independently
% from the homework solution.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1a                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Implementing a counter.
% We are going to use a dynamic predicate called counter/1 to
% store the current value of the counter.

% Set the counter to the value specified (by first retracting
% any previous assertions regarding counter/1, and then
% asserting the appropriate fact):

set_counter(Value) :-	
  retractall(counter(_)),
  assert(counter(Value)).

% Inititalising the counter means setting the counter to 0:

init_counter :-
  set_counter(0).

% Retrieve the current value of the counter:

get_counter(Value) :-
  counter(Value).

% Increment the counter by 1:

step_counter :-
  get_counter(Value),
  NewValue is Value + 1,
  set_counter(NewValue).

% At compilation time, initialise the counter. This avoids
% error messages in case get_counter/1 is used before
% init_counter/0 has been executed by the user or by another
% module of the program.

:- init_counter.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1b                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Run an experiment for a given sorting algorithm and a 
% given list of integers and return the number of primitive
% comparison operations required. The ordering relation is
% always going to be <.

% We first change the implementation of check/3 in such a 
% way that each time that predicate is called, the counter 
% gets incremented. This implementation overrides the
% implementation of check given in sort.pl.

check(Rel, A, B) :-
  step_counter,
  Goal =.. [Rel,A,B],
  call(Goal).

% Implementation of experiment/3: In the first line of the
% rule body the goal is being composed. The functor of the
% goal will be the name of the chosen algorithm, the first
% argument will be the ordering relation <, and the third
% will be the list provided. As we are not actually
% interested in the sorted list itself, we use the anonymous
% variable for the final argument of the goal. Then we
% initialise the counter, execute the goal, and finally
% return the new value of the counter as the answer.

experiment(Algorithm, List, Count) :-
  Goal =.. [Algorithm,<,List,_],
  init_counter,
  call(Goal),
  get_counter(Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1c                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generate a list of a given length containing random
% integers between 1 and a given maximum. For length 0, the
% answer is the empty list (base case). For the recursive
% rule, we first use the built-in function random/1 to
% generate a new random number. This will be the head of the
% output list. Then we count down the length parameter and
% call random_list/3 again recursively to generate the tail
% of the output list.

random_list(0, _, []).

random_list(Length, MaxElem, [Head|Tail]) :-
  Length > 0,
  NewLength is Length - 1, 
  Head is random(MaxElem) + 1,
  random_list(NewLength, MaxElem, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1d                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Running an experiment on a random list:
% We use random_list/3 to generate a random list of the
% specified length, and then experiment/3 to run an
% experiment on that list. 

random_experiment(Algorithm, Length, MaxElem, Count) :-
  random_list(Length, MaxElem, List),
  experiment(Algorithm, List, Count).

% Running a number of experiments on random lists:
% The predicate random_experiments_list/5 runs the specified
% number of experiments using random_experiment/4 and
% collects the answers (counts) in a list. Then the main
% predicate computes the mean of that list of integers,
% rounds that number to the nearest integer, and return this
% as the answer. The predicate mean/2 is used to calculate the
% the arithmetic mean (i.e., the average) of a list of integers.

random_experiments(Algorithm, Length, MaxElem, Number, AvgCount) :-
  random_experiments_list(Algorithm, Length, MaxElem, Number, Counts),
  mean(Counts, Average),
  AvgCount is round(Average).

% The cut in the following predicate is not necessary from a
% "logical" point of view (there would be no wrong answers
% during backtracking, for instance). It is included here
% solely  for the purpose of making the program more
% efficient. If a recursive rule involves only a single
% recursive call, that recursive call is the last subgoal 
% in the rule body, and there is a cut preceding that last
% subgoal, then Prolog can manage the memory required to 
% keep track of the current state of the recursion stack in
% a more efficient manner (because it knows for sure that
% backtracking will not be required later on). This is known
% as "tail recursion" (see Bratko, Section 8.5.3 for more
% information).

random_experiments_list(_, _, _, 0, []).

random_experiments_list(Algorithm, Length, MaxElem, Number, [Count|Counts]) :-
  Number > 0,
  random_experiment(Algorithm, Length, MaxElem, Count),
  NewNumber is Number - 1, !, % cut for efficiency only
  random_experiments_list(Algorithm, Length, MaxElem, NewNumber, Counts).

% The predicate mean/2 can be used to compute the arithmetic
% mean of a given list of numbers. Note that the base case is
% not the empty list, because without any numbers you cannot
% calculate a mean (the mean would be undefined in that case).
% In the recursion step we take the mean of the tail,
% multiply it with the length of the tail (this will give us
% the sum of all elements but the head), add the head, and
% finally divide this by the length of the entire list.

mean([N], N).

mean([N | Tail], Result) :-
  mean(Tail, TailResult),
  length(Tail, TailLength),
  Result is (TailResult * TailLength + N) / (TailLength + 1).

% An alternative solution would be to first compute the sum
% of the numbers in the list and then to divide by the length
% of the list. (Note that this is Exercise 3.5 from the Prolog Notes.)

% Runtimes (sorting 100 numbers from 1 to 500):
% Here are a few sample queries that show both the average
% number of comparisons required and the number of milliseconds
% required (for 100 runs).

% ?- time(random_experiments(bubblesort, 100, 500, 100, Count)).
% 132,483,526 inferences, 43.45 CPU in 43.58 seconds (100% CPU, 3049103 Lips)
% Count = 110162
% Yes

% ?- time(random_experiments(bubblesort2, 100, 500, 100, Count)).
% 10,740,496 inferences, 3.51 CPU in 3.53 seconds (99% CPU, 3059970 Lips)
% Count = 8885
% Yes

%?- time(random_experiments(quicksort, 100, 500, 100, Count)).
% 897,843 inferences, 0.31 CPU in 0.32 seconds (98% CPU, 2896268 Lips)
% Count = 653
% Yes

% That is, it takes over 100,000 comparisons and just under 1/2  
% a second to sort a list of 100 elements using naive bubblesort.
% For improved bubblesort, these figures improve by a factor of
% about 12 (~8,800 comparisons and ~35ms). Using quicksort, we
% get a further improvement by again a similar factor (~650
% comparisons and ~3ms). So quicksort is almost 150 times faster
% than naive bubblesort on a list of 100 random elements. For a
% list of only 10 elements, on the other hand, quicksort is only
% around 4 to 5 times as fast as bubblesort (try it). This
% exemplifies that the difference in complexity that we have
% observed at a theoretical level only really starts to matter as
% the problem size increases.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1e                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Visualise the runtimes required to sort random lists of
% different length. The input parameters are the name of the
% algorithm to be used, the maximum length of the list, the
% maximum random integer, and the number of experiments to
% be run for each list length.

% We use an auxiliary predicate chart/5 to implement chart/4.
% The additional parameter is a counter, counting up from 1
% up to the maximum list length. For each value of the
% counter, random_experiments/5 is called to compute the
% average number of comparisons required. Then a line of
% asterisks of that length is printed on the screen. This is
% done using the predicate line/2. We also print out the 
% length of the list tested for each line. In this context, the
% predicate rightflush_number/2, explained below, is used to
% make sure that numbers of varying length all take up the
% same width (i.e., a certain number of blank characters will
% be printed first).

chart(Algorithm, MaxLength, MaxElem, Number) :-
  chart(Algorithm, 1, MaxLength, MaxElem, Number).

chart(_, Length, MaxLength, _, _) :-
  Length > MaxLength.

chart(Algorithm, Length, MaxLength, MaxElem, Number) :-
  random_experiments(Algorithm, Length, MaxElem, Number, Count),
  rightflush_number(Length, MaxLength),
  write(' > '), line(Count, '*'), nl,
  NewLength is Length + 1,
  chart(Algorithm, NewLength, MaxLength, MaxElem, Number).

% Given two numbers, the second of which should not be
% smaller than the first, print out the first number preceded
% by as many blank characters as are needed to take up the
% same space as printing the second number would. This is
% done by first computing the number of digits for both
% numbers and then using line/1 to print out the
% appropriate number of blank characters.

rightflush_number(Number, BigNumber) :-
  digits(Number, NumberDigits),
  digits(BigNumber, BigNumberDigits),
  Shift is BigNumberDigits - NumberDigits,
  line(Shift, ' '),
  write(Number).

% Compute the number of digits of a given positive integer.
% Explanation: The logarithm with base 10 gives a first
% approximation. For instance, we have log_10(1000)=3 and
% log_10(999)=2.99... The floor/1 function returns the
% integer we get by removing anything after the decimal
% point.

digits(Number, Digits) :-
  Digits is floor(log10(Number)) + 1.

% Print a given number of copies of a given string:

line(0, _).
	
line(Length, Char) :-
  write(Char),
  Length1 is Length - 1,
  line(Length1, Char). 

% Here are some sample runs:

/*************************************************************

?- chart(bubblesort, 8, 50, 100).
1 >
2 > *
3 > ****
4 > *********
5 > ***************
6 > *************************
7 > **************************************
8 > *********************************************************
Yes

?- chart(bubblesort2, 10, 50, 100).
 1 >
 2 > **
 3 > ****
 4 > ********
 5 > **************
 6 > *********************
 7 > ******************************
 8 > ******************************************
 9 > ****************************************************
10 > *******************************************************************
Yes
  
?- chart(quicksort, 15, 50, 100).
 1 >
 2 > *
 3 > ***
 4 > *****
 5 > *******
 6 > **********
 7 > **************
 8 > ******************
 9 > *********************
10 > *************************
11 > *****************************
12 > ********************************
13 > *************************************
14 > ******************************************
15 > **********************************************
Yes

*************************************************************/

% These graphs confirm the theoretical complexity results
% discussed in class. For example, for bubblesort2, we clearly
% see a typical quadratic function (a parabola), corresponding
% to the O(n^2) complexity of the improved bubblesort algorithm.
% For simple bubblesort, we see a similar curve that increases
% significantly faster. For quicksort, we see a curve that
% grows much more slowly and that is much closer to a linear
% function, reflecting the O(n log n) complexity of quicksort.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We use Prolog atoms to represent the four items involved in
% the fox-goat-cabbage puzzle: fox, goat, cabbage, farmer.
% We represent states as pairs of lists of the form Left/Right,
% where Left is the list of items on the lefthand side of the
% river and Right is the list of items on the righthand side
% of the river. For example, [fox,goat,farmer]/[cabbage] is
% the state where only the cabbage is on the righthand side.

% We first implement an auxiliary predicate to check whether a
% given list of items is safe, i.e., whether it either includes
% the farmer, or does not include a problematic pair of items
% (which are fox/goat and goat/cabbage).

safe(Set) :-  
  member(farmer, Set).	 

safe(Set) :-
  \+ (member(cabbage,Set), member(goat,Set)),
  \+ (member(goat,Set), member(fox,Set)).  

% There are four types of move available to the farmer.
% He can move either from the left to the right or from the
% right to the left, and he can either move on his own or
% take one of the three items with him. We implement one rule
% for move/2 for each of these four cases. To be able to move
% from the left to the right, for instance, we have to
% succeed in selecting the farmer from amongst the items in
% the lefthand list. If the farmer does not move alone, we
% furthermore have to succeed in selecting a further item
% from the lefthand list. We then obtain the new state by
% equating the new lefthand list with the remainder of the
% old lefthand list after these selection operations, and we
% obtain the new righthand list by adding the selected item(s)
% to the old righthand list. Finally, we need to check that
% the new lefthand list (which does not include the farmer)
% is still safe. 

% Farmer moving from left to right alone:
move(Left/Right, NewLeft/NewRight) :- 
  select(farmer, Left, NewLeft),	 
  NewRight = [farmer|Right], 
  safe(NewLeft).  

% Farmer moving from left to right with one item:
move(Left/Right, NewLeft/NewRight) :- 
  select(farmer, Left, Left1),	
  select(Item, Left1, NewLeft),	 
  NewRight = [farmer,Item|Right], 
  safe(NewLeft).  

% Farmer moving from right to left alone:
move(Left/Right, NewLeft/NewRight) :- 
  select(farmer, Right, NewRight),	 
  NewLeft = [farmer|Left], 
  safe(NewRight).

% Farmer moving from right to left with one item:
move(Left/Right, NewLeft/NewRight) :-  
  select(farmer, Right, Right1),  
  select(Item, Right1, NewRight), 
  NewLeft = [farmer,Item|Left], 
  safe(NewRight).	

% We have reached our goal once all four items are on the
% righthand side of the river, i.e., once the lefthand list
% is empty. The additional check below of all four items
% being members of the righthand list is, strictly speaking,
% redundant (as long as we are certain that our move/2
% predicate works correctly and no items get lost along the
% way).

goal(Left/Right) :-  
  Left = [],  
  member(cabbage, Right), 
  member(goat, Right), 
  member(fox, Right), 
  member(farmer, Right).

% We are now able to solve the puzzle by searching the state
% space defined for a path from the initial state where all
% four items are on the lefthand side, i.e., with the initial
% state being [farmer,cabbage,goat,fox]/[].

% Of the three depth-first search algorithms we have seen,
% the cycle-free variant without a bound on the depth is the
% right choice. First, the one with a bound on the depth has
% the disadvantage that we have to guess an appropriate bound
% first. Second, the one without cycle-detection does not work,
% as it results in an infinite loop (because the farmer could,
% for instance, keep going back and forth with the goat).
% Breadth-first search and iterative deepening (to be
% introduced in the next lecture) also work. Here is the
% query producing the result:

% ?- solve_depthfirst_cyclefree([farmer,cabbage,goat,fox]/[], Plan).
% Plan = [[farmer, cabbage, goat, fox]/[], [cabbage, fox]/[farmer, goat], [farmer, cabbage, fox]/[goat], [fox]/[farmer, cabbage, goat], [farmer, goat, fox]/[cabbage], [goat]/[farmer, fox|...], [farmer|...]/[fox|...], []/[...|...]]
% Yes

% Unfortunately, this is not very readable (even if we use
% write/1 to see the full list representing the plan). The
% following predicate allows us to display such a plan in a
% more reader-friendly manner:

show_plan([]).

show_plan([Left/Right|Plan]) :-
  write('Lefthand side:  '), write(Left), nl,
  write('Righthand side: '), write(Right), nl, nl,
  show_plan(Plan).

% Now we can use the following query:

% ?- solve_depthfirst_cyclefree([farmer,cabbage,goat,fox]/[], Plan), show_plan(Plan).
% Lefthand side:  [farmer, cabbage, goat, fox]
% Righthand side: []
%
% Lefthand side:  [cabbage, fox]
% Righthand side: [farmer, goat]
%
% Lefthand side:  [farmer, cabbage, fox]
% Righthand side: [goat]
%
% Lefthand side:  [fox]
% Righthand side: [farmer, cabbage, goat]
% 
% Lefthand side:  [farmer, goat, fox]
% Righthand side: [cabbage]
%
% Lefthand side:  [goat]
% Righthand side: [farmer, fox, cabbage]
% 
% Lefthand side:  [farmer, goat]
% Righthand side: [fox, cabbage]
%
% Lefthand side:  []
% Righthand side: [farmer, goat, fox, cabbage]
% 
% Plan = [[farmer, cabbage, goat, fox]/[], [cabbage, fox]/[farmer, goat], [farmer, cabbage, fox]/[goat], [fox]/[farmer, cabbage, goat], [farmer, goat, fox]/[cabbage], [goat]/[farmer, fox|...], [farmer|...]/[fox|...], []/[...|...]]
% Yes  

% That is, the farmer has to cross the river 7 times to solve
% the puzzle. We can use the depth-bounded version of our
% depth-first search algorithm to check that there indeed is
% no shorter solution. In the two queries below we use the
% anonymous variable, as we are not interested in the actual
% plan generated, but only in whether such a plan exists at all:
  
% ?- solve_depthfirst_bound(7, [farmer,cabbage,goat,fox]/[], _).
% Yes

% ?- solve_depthfirst_bound(6, [farmer,cabbage,goat,fox]/[], _).
% No

% Enforced backtracking on the first of the above queries
% shows that there in fact are exactly two solutions
% consisting of 7 steps each (the one where the fox is the
% second item to move, as in the solution displayed above,
% and the one where instead the cabbage is the second item
% to move across the river).  



