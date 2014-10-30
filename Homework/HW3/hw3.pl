%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logisch Programmeren en Zoektechnieken 2014               %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Homework #3: Answers                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Counting the occurrences of a given element in a list.
% This is another example for a recursion with one base
% case and two recursive rules. If the list is empty,
% whatever the element may be (anonymous variable), the
% number of occurrences has to be 0. For the recursion
% steps we use the head/tail-pattern. If the element we
% are looking for matches the head, increase the counter,
% otherwise don't. We need a cut in the first rule to
% make sure the second one will not be used instead when
% Prolog tries to backtrack.

occurrences(_, [], 0).

occurrences(Head, [Head | Tail], N) :- !,
  occurrences(Head, Tail, N1),
  N is N1 + 1.

occurrences(Element, [_ | Tail], N) :-
  occurrences(Element, Tail, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We first define appropriate operators for propositional logic. 
% Negation is defined as a unary prefix operator. We choose the 
% associativity pattern fy to make sure that double negations, 
% as in neg neg p, can be written without using parentheses (if 
% you choose the pattern fx instead, then one of the examples 
% given in the exercise will not work). Conjunction and 
% disjunction are defined as left-associative infix operators 
% (using pattern yfx). We define conjunction as more strongly 
% binding than disjunction, which is the common convention. 
% That is, p or q and r is interpreted as p or (q and r).

:- op(100, fy, neg), 
   op(200, yfx, and), 
   op(300, yfx, or).

% We implement nnf/1, for checking whether a given formula is 
% in negation normal form, using a simple recursive approach.

% A conjunction is in NNF if both of its conjuncts are:
nnf(Left and Right) :- 
  nnf(Left),
  nnf(Right).

% A disjunction is in NNF if both of its disjuncts are:
nnf(Left or Right) :- 
  nnf(Left),
  nnf(Right).

% A negated formula is in NNF only if its immediate subformula 
% is atomic (which we can test using the built-in atom/1):
nnf(neg Atom) :- 
  atom(Atom).

% In all other cases the given formula can only be in NNF if 
% it is in fact an atomic formula:
nnf(Atom) :-
  atom(Atom).

% Note: We could slightly improve the efficiency of this 
% program by inserting a cut directly after the head of the 
% first three rules (because we know that once we have matched 
% the head, there certainly will be no other relevant rule 
% anymore that could be tried instead). This would be a good 
% use of cuts, but it is not strictly needed here. 
% Alternatively, if we do use those cuts, then we could replace 
% the final rule with the fact nnf(_), as in that case we would 
% be sure that it would only get reached in case the argument is 
% an atom, i.e., we would not have to test this anymore. 
% However, the slight improvement in efficiency thus gained 
% probably is not worth the risk of introducing mistakes when 
% changing the program this way (the fact nnf(_) in isolation 
% clearly is not very meaningful, as it suggests that anything 
% is in NNF---that is, our alternative solution with cuts is 
% lacking the declarative nature of the first solution).

% The following predicate nnf/2 can be used to translate a 
% given arbitrary formula into an equivalent formula in NNF. 
% It works by recursively applying de Morgan's laws (for 
% rewriting negated conjunctions into disjunctions of negated 
% subformulas and for rewriting negated disjunctions into 
% conjunctions of negated subformulas) and the rule for 
% eliminating double negations (saying that a formula of the 
% form (neg neg A) is equivalent to A).

% Elimination of double negations:
nnf(neg neg A, NNF) :- !,
  nnf(A, NNF).

% De Morgan's law for negated conjunctions:
nnf(neg(A and B), NNF) :- !,
  nnf((neg A) or (neg B), NNF).

% De Morgan's law for negated disjunctions:
nnf(neg(A or B), NNF) :- !,
  nnf((neg A) and (neg B), NNF).

% Translate a conjunction by translating its conjuncts:
nnf(A and B, A_NNF and B_NNF) :- !,
  nnf(A, A_NNF),
  nnf(B, B_NNF).

% Translate a disjunction by translating its disjuncts:
nnf(A or B, A_NNF or B_NNF) :- !,
  nnf(A, A_NNF),
  nnf(B, B_NNF).

% If we make it this far, it must be a literal (and the 
% translation of a literal is just that same literal):
nnf(Lit, Lit).

% Here are a couple of examples of nnf/2 in action:

% ?- nnf(neg(neg p or neg q), Result).
% Result = p and q
% Yes

% ?- nnf(neg(a or (neg (c and neg neg neg d))), Result).
% Result = neg a and (c and neg d)
% Yes

% Note that implementing nnf/2 was entirely optional: we do 
% not give any points for it, nor do we take off points for 
% not attempting to implement it.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 3                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Here are again the facts specifying the so-called voting game 
% corresponding to the rules laid down in the Treaty of Rome:

countries([belgium, france, germany, italy, luxembourg, netherlands]).
weight(france, 4).
weight(germany, 4).
weight(italy, 4).
weight(belgium, 2).
weight(netherlands, 2).
weight(luxembourg, 1).
threshold(12).

% We first write a predicate to compute the sum of weights for 
% a given list of countries. The base case is the empty list of 
% countries, resulting in weight 0. In the recursive rule we 
% retrieve the weight of the country that is the head of the 
% input list and then add that weight to the sum of weights of 
% the countries corresponding to the tail, which we compute 
% recursively.

sum_of_weights([], 0).

sum_of_weights([Head|Tail], Sum) :-
  weight(Head, Weight),
  sum_of_weights(Tail, TailSum),
  Sum is TailSum + Weight.

% Now we can define a coalition of countries as winning if 
% their sum of weights is at least equal to the threshold  
% (the latter of which we retrieve by querying threshold/1):

winning(Coalition) :-
  sum_of_weights(Coalition, Sum),
  threshold(Threshold),
  Sum >= Threshold.

% Our definition of the predicate critical/2, for checking 
% whether a given country is critical for a given coalition, 
% is a simple line-by-line translation of the three conditions 
% specified in the exercise:

critical(Country, Coalition) :-
  \+ member(Country, Coalition),
  \+ winning(Coalition),
  winning([Country|Coalition]).

% The predicate sublist/2 is implemented recursively. The 
% only sublist of the empty list is the empty list itself 
% (base case). We provide two recursive rules: The first 
% copies the head of the input list into the head of the 
% sublist to be produced and then continues with looking 
% for a sublist of the tail of the input list. The second 
% recursive rule instead drops the head of the input list, 
% and then also recursively looks for a sublist of the 
% input tail. (This is a particularly good example for the 
% power of recursion combined with backtracking, which 
% allows us to implement a rather complex concept in just 
% a few lines of simple code.)

sublist([], []).  

sublist([X|SubList], [X|List]) :-
  sublist(SubList, List).

sublist(SubList, [_|List]) :-
  sublist(SubList, List).

% As shown in the exercise, we can now use a simple query to 
% compute all coalitions that are critical for a given country 
% through enforced backtracking. Specifically, suppose the 
% variable Country is bound to our country of interest and 
% suppose the variable Countries is bound to the list of all 
% countries. Then executing the following goal (consisting of 
% two subgoals) will succeed if the variable Coalition can 
% be instantiated with a coalition that is critical for our 
% country:
%
% sublist(Coalition, Countries), critical(Country, Coalition)
%
% Enforced backtracking will return all critical coalitions 
% for our country. Armed with this insight, it is now easy to 
% compute the voting power of a given country by first using 
% findall/3 to collect all answers to the above kind of goal 
% in a list (i.e., assembling all critical coalitions for a 
% given country) and then measuring the length of that list:

voting_power(Country, Power) :-
  countries(Countries),
  Goal = (sublist(Coalition,Countries), critical(Country,Coalition)),
  findall(Coalition, Goal, Coalitions),
  length(Coalitions, Power).

% The voting power of Germany is 10 and that of Luxembourg is 0:

% ?- voting_power(germany, Power).
% Power = 10
% Yes

% ?- voting_power(luxembourg, Power).
% Power = 0
% Yes

% The latter means that there is no possible scenario in 
% which a coalition of countries would like to pass a proposal, 
% and is too weak to do so without Luxembourg but sufficiently 
% strong with Luxembourg. That is, Luxembourg really has no 
% voting power under the rules of the Treaty of Rome. 

% Note: What I called the voting power of a country is a 
% simplified version of the so-called Banzhaf power index, 
% an important concept that is widely used in political 
% science and economics. In recent years the study of so-called
% coalitional games (of which the voting rule specified in the
% Treaty of Rome is an example), have become a popular research
% topic in AI. One reason is that they are useful to model what
% a group of intelligent agents can or cannot achieve together
% (which is a crucial issue in multiagent systems, one of the
% largest active research areas in AI today). Another reason is
% that they generate interesting algorithmic challenges. For
% example, if you have, say, 20 countries, then there are
% 2^20 > 1 million potential coalitions to consider; so you
% can imagine that things quickly get rather complex and
% computing the value of a power index becomes a hard problem. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 4                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Matching: =/2
% A goal of the form A = B succeeds if the terms A and B match, 
% i.e., if they can be made identical by replacing any of the 
% variables in A and B by suitable ground terms. (Note: At the 
% end of the course we will briefly discuss the relationship 
% between pattern matching as implemented in Prolog and 
% represented by =/2 and fully-fledged logical unification, 
% which is a little different but also fits the---slightly 
% underspecified---definition given here.)

% Example:
% ?- test(X, b) = test(a, b).
% X = a
% Yes

% Identity: ==/2
% A goal of the form A == B succeeds if the terms A and B are 
% identical, also as far as the names of (as yet uninstantiated)
% variables are concerned.

% Example:
% ?= X == Y.
% No

% Arithmetic equivalence: =:=/2
% A goal of the form A =:= B succeeds if the terms A and B 
% are both fully instantiated arithmetic expressions that 
% evaluate to the same number.

% Example:
% ?- member(X, [2,5]), X * X =:= X + X.
% X = 2 ;
% No

% Arithmetic evaluation: is/2
% A goal of the form A is B succeeds if B is a fully 
% instantiated arithmetic expression and A can be matched 
% with the number that is the result of evaluating B. 
% (In practice, A should always be a variable, i.e., the 
% is-operator should not be used for arithmetic comparisons, 
% as this can easily lead to mistakes, such as unintended 
% failures due to attempted matchings of integers with floats).

% Example:
% ?- X is 3 + 5.0.
% X = 8.0
% Yes

% Term (de)composition: =../2
% A goal of the form A =.. B succeeds if A is (or can be 
% matched with) a compound term and B is (or can be matched 
% with) a list, such that the functor of the former matches 
% the head of the latter and the arguments of the former 
% match the elements in the tail of the latter.

% Example:
% ?- X =.. [test,elephant,4].
% X = test(elephant, 4)
% Yes




