%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logisch Programmeren en Zoektechnieken 2014               %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Homework #1: Answers                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% As you will have guessed, the point of this exercise was
% to make sure that everyone knows who their TA is and has
% talked to them at least once. The programming involved is
% straightforward. Here is the pogram I used to generate the
% example given on the problem sheet:

answer(Question):-
  database(Question, Answer),
  write(Answer).

database(name, 'Joost').
database(pancake, 'Appelstroop').
database(world_cup, 'Apeldoorn').

% Alternatively, you could have written three rules for
% answer/1, that each directly execute the write-command.
% My solution is maybe slightly prettier, as it separates the
% list of answers for each question and the instruction for
% what to do with a given answer (namely, to print it to the
% screen), whatever the question and answer concerned might
% be. So my solution is a bit more modular, and if we extend
% the program to handle more question-answer pairs, it will
% also be shorter than the other solution.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This one's easy. We just have to match the argument with
% either the empty list or the head/tail-pattern.

analyse_list([]) :-
  write('This is an empty list.').

analyse_list([Head|Tail]) :-
  write('This is the head of your list: '),
  write(Head),
  nl,
  write('This is the tail of your list: '),
  write(Tail).

% Note that we do not need an extra rule to deal with the
% case of an input argument that isn't a list. We get this 
% to work correctly for free: Prolog will simply fail when
% provided with such a query.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 3                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We represent unary numbers as lists of x's of the appropriate
% length. For example, 5 is represented as [x,x,x,x,x]. One
% approach to implementing arithmetic operations would be to
% first measure the length of a given list (to translate from
% the list-representation to numbers), to then do the required
% arithmetic operations using normal operations for normal
% numbers, and to finally translate back to the list-representation
% (by creating a list of the given length). But this was not the
% purpose of this exercise. Here we provide all these arithmetic
% operations without making any reference to normal numbers or
% normal built-in arithmetic operations.

% We obtain the sussessor of a unary number represented as a list
% by adding one more x to that list. The easiest way of doing this
% is to use the head/tail-pattern.

successor(Number, [x|Number]).

% Note that you can also use this implementation to compute the
% predecessor of a given positive number. For example, the predecessor
% of three is two:

% ?- successor(N, [x, x, x]).
% N = [x, x]
% Yes

% Adding two unary number represented as lists simply means
% appending those two lists.

plus(Number, OtherNumber, Sum) :-
  append(Number, OtherNumber, Sum).

% You can also use this implementation to subtract numbers. For
% example, to find out what eight minus five is, you can use the
% following query: 

% ?- plus([x, x, x, x, x], Rest, [x, x, x, x, x, x, x, x]).
% Rest = [x, x, x]
% Yes

% Here is an alternative implementation of the predicate for addition
% that can do without append/3. It is based on the following idea.
% First, adding zero to any number M results in that number M (base
% case). Second, adding the number N to the number M will give the
% same result as adding N-1 to M+1 (recursive rule, with the first
% number getting reduced by one in each step).

plus2([], Number, Number).

plus2([x|Number], OtherNumber, Sum) :-
  plus2(Number, [x|OtherNumber], Sum).

% We can multiply two numbers using the following recursive
% approach. First, we know that multiplying zero (empty list)
% with any other number (anonymous variable) always yields zero
% (empty list). This is the base case. Second, to multiply N
% with M, we can first multiply N-1 with M, and then add M to
% this intermediate result. This is our recursive rule.
 
times([], _, []).

times([x|NumberMinusOne], OtherNumber, Product) :-
  times(NumberMinusOne, OtherNumber, FirstProduct),
  plus(FirstProduct, OtherNumber, Product).

% To a limited extent, you can also use this implementation
% to divide one number by another. If the expected result is
% an integer and if you do not ask Prolog for alternative
% solutions, then it works perfectly. Here is six divided by
% three:

% ?- times([x, x, x], Result, [x, x, x, x, x, x]).
% Result = [x, x]
% Yes

% But if you try more complicated queries, you may end up in
% an infinite loop. Experiment around with a few such queries
% and try to understand what happens. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 4                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Note that what we have called bulldozing is actually known
% under the name of flattening. SWI-Prolog comes with a
% built-in predicate called flatten/2, which is in fact
% better than the solution given below (it can also handle
% lists of variables, for instance, which was not required
% for this question).

% We give a recursive implementation. The base case is the
% case of the empty list: bulldozing the empty list results
% again in an empty list. Then we write one recursive rule
% for each of the two remaining cases: (1) If the head of
% our big list is itself an list, then we bulldoze both that
% list in the head position and the list represented by the tail
% and obtain the final result by appending the two. (2) If the
% head of our big list is any other (ground) term that is not a
% list, then that head will also be the head of the bulldozed
% output list, and we recursively apply bulldoze/2 to the tail
% of the big list.

bulldoze([], []).

bulldoze([Head|Tail], BulldozedList) :-
  list(Head),
  bulldoze(Head, BulldozedHead),
  bulldoze(Tail, BulldozedTail),
  append(BulldozedHead, BulldozedTail, BulldozedList).

bulldoze([Head|Tail], [Head|BulldozedTail]) :-
  bulldoze(Tail, BulldozedTail).

% We use the following auxiliary predicate to check whether a
% given term is a list (by checking whether it is either the
% empty list or matches the head/tail-pattern).

list([]).

list([_|_]).

% Note that SWI-Prolog comes with a built-in predicate called
% is_list/2 that has the same functionality, but in addition
% works correctly also in case the term given is a variable
% (that is, it fails in that case).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 5                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Removing duplicates from the empty list again yields
% the empty list (base case). We have to consider two
% cases for the recursion step. Either the head of the
% list can again be found in the tail (in which case the
% head is dropped) or it can't (in which case it is kept).

remove_duplicates([], []).

remove_duplicates([Head | Tail], Result) :-
  member(Head, Tail),
  remove_duplicates(Tail, Result).

remove_duplicates([Head | Tail], [Head | Result]) :-
  remove_duplicates(Tail, Result).

% Note that in this version only the first of all
% alternative solutions is correct! Later in the course
% you will learn how this can be improved using cuts.

% Another solution:
% We use the built-in predicate delete/3. In this version
% Prolog will always compute just one (correct) solution.

remove_duplicates2([], []).

remove_duplicates2([Head | Tail], [Head | Result]) :-
  delete(Tail, Head, Tail1),
  remove_duplicates2(Tail1, Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 6                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Reversing the empty list again yields the empty list
% (base case). Otherwise, reverse the tail and add the
% head to the result of reversing the tail (recursion
% step). For this we can use append/3. But note that we
% have to use [Head], not just Head, i.e., we're turning
% it into a list before using it with append/3.

reverse_list([], []).

reverse_list([Head | Tail], ReversedList) :-
  reverse_list(Tail, ReversedTail),
  append(ReversedTail, [Head], ReversedList).

% The above is a very simple but not the most efficient
% implementation of reverse_list/2. The problem is that
% append/2 is itself a rather "expensive" predicate: using
% it to append Head at the end of ReversedTail gives rise
% to another recursive call within every recursive call of
% reverse_list/2. Here's a more efficient implementation:

reverse_list2(List, ReversedList) :-
  reverse_aux(List, [], ReversedList).

% It uses the auxiliary predicate reverse_aux/3. In this
% implementation we use the "accumulator" in the second
% argument to collect the elements of the reversed list
% (recursion step). Once there are no more elements left
% in the input list, we copy the content of the accumulator
% to the third argument (base case).

reverse_aux([], ReversedList, ReversedList).

reverse_aux([Head | Tail], Accumulator, ReversedList) :-
  reverse_aux(Tail, [Head | Accumulator], ReversedList).

% You can get an idea of the difference in performance
% using the time/1 predicate. The difference for small
% examples in not significant enough to be measurable
% in hundredths of a second, but you can see a difference
% if you look at the number of inference steps reported
% by SWI-Prolog (36 versus 9 in this example):

% ?- time(reverse_list([1,2,3,4,5,7,8], X)).
% 36 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
% X = [8, 7, 5, 4, 3, 2, 1]
% Yes

% ?- time(reverse_list2([1,2,3,4,5,7,8], X)).
% 9 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
% X = [8, 7, 5, 4, 3, 2, 1]
% Yes


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 7                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Below is the big animal program after having changed the
% order of the subgoals in the second rule:

bigger(elephant, horse).
bigger(horse, donkey).
bigger(donkey, dog).
bigger(donkey, monkey).

is_bigger(X, Y) :- bigger(X, Y).
is_bigger(X, Y) :- is_bigger(Z, Y), bigger(X, Z). % changed!

% Prolog's reply to the query is_bigger(A, donkey) is this:

% ?- is_bigger(A, donkey).
% A = horse ;
% A = elephant ;
% ERROR: Out of local stack

% Explanation: Once there are no more correct solutions, Prolog 
% will keep replacing goals of the form is_bigger(_, _) with
% new subgoals of the same form and enter an infinite loop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
