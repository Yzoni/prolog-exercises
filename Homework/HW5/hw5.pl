%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logisch Programmeren en Zoektechnieken 2014               %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Homework #5: Answers                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Declare goal/1 as being discontiguous, so we can use it for
% both Question 1 and 2, without triggering a warning message:

:- discontiguous(goal/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Make the uninformed search algorithms available:

:- consult('search.pl').

% We represent states as expressions of the form Target:Terms,
% where Target is the target number provided and Terms is a
% list of arithmetic terms we have available for use.
% For example, 297:[(100+75)/25,50*6,4] represents the state
% in which we have the three terms (100+75)/25, 50*6, and 4
% available and in which we are supposed to construct a term
% that evaluates to 297. 

% A move involves selecting two terms from our list and combining 
% them into a new term (by joining them together with a suitable 
% arithmetic operation), and then replacing the two old terms by 
% the new term thus obtained. We use the built-in select/3 twice 
% to extract the two terms; the remaining list is called Terms2. 
% Finally, we use the head/tail-pattern to insert the new Term
% into our remaining list of terms. The task of building a new
% term from two given terms X and Y is delegated to the predicate
% build_terms/3 implemented below.

move(Target:Terms, Target:[Term|Terms2]) :-
  select(X, Terms, Terms1),
  select(Y, Terms1, Terms2),
  build_term(X, Y, Term).

% The predicate build_terms/3 takes two arithmetic terms as input 
% and returns a new arithmetic term combining these two input
% terms using a suitable arithmetic operation as output
% The available arithmetic operations are addition (+),
% subtraction (-), multiplication (*), and division (/).
% In case of division, we need to check that the divisor used is 
% not arithmetically equivalent to 0 and we also need to check 
% that the remainder of the integer division would be 0.

build_term(X, Y, X + Y).
build_term(X, Y, X - Y).
build_term(X, Y, X * Y).
build_term(X, Y, X / Y) :- Y =\= 0, X mod Y =:= 0.

% We have reached our goal if the list of terms in the current
% state include a term that is arithmetically equivalent to the
% target number.

goal(Target:Terms) :-
  member(Term, Terms),
  Target =:= Term.

% To solve the game, we apply one of our search algorithms to
% the initial state, which we can construct from the given target
% number and the given list of numbers. We are not interested in
% the full path the the goal state, but only in the goal state
% itself, the last element of the list representing the path.
% The goal state will be of the form Target:FinalTerms. We know
% that FinalTerms must include a term that is arithmetically
% equivalent to the target number. We extract that term and return
% it in the third argument position as the solution to the game.

solve(Target, Numbers, Term) :-
  solve_iterative_deepening(Target:Numbers, Path),  
%  solve_breadthfirst(Target:Numbers, Path),
%  solve_depthfirst(Target:Numbers, Path),
%  solve_depthfirst_cyclefree(Target:Numbers, Path),
  last(Path, _:FinalTerms),
  member(Term, FinalTerms),
  Target =:= Term.

% You can easily switch between different search algorithms by
% changing the code above.

% Below are three examples, all of them generated using iterative 
% deepening. The first one is taken from the edition of Cijfers en 
% Letters broadcast on 8 August 1987. During the show, the resident 
% /cifer expert/ failed to find a solution, which prompted show 
% master Bob Bouma to declare: "het is dus niet te maken". But in 
% fact it is:

% ?- solve(505, [8,1,8,10,4,7], Solution).
% Solution = (8+10)*4*7+1 
% Yes

% The second example comes from the edition of Countdown aired on 
% 14 March 1997. Contestant James Martin managed to find the answer 
% found also by our program. The show's co-host Carol Vorderman 
% called it "probably the most incredible answer I ever had". On my 
% machine it takes Prolog around 71 seconds to find the solution 
% (so we would have lost against this particular human contestant).

% ?- solve(952, [25,50,75,100,3,6], Solution).
% Solution = ((100+6)*(75*3)-50)/25 
% Yes

% Our final example is taken from the 40-year anniversary edition 
% of Des Chiffres et Des Lettres broadcast on 6 January 2013. 
% Prolog returns a solution in about 1 second (the two celebrity 
% contestants on the show did not manage to solve the game):

% ?- solve(284, [2,50,4,4,8,7], Solution).
% Solution = 4-(2-50+8)*7 
% Yes

% Iterative deepening generally works well for this kind of problem, 
% while breadth-first search almost never manages to find a solution. 
% This is due to the high space complexity of breadth-first search. 
% To see that space really is the issue here, we can artificially 
% lower the space available to Prolog:

% ?- set_prolog_stack(global, limit(1000000)).
% Yes

% Now Prolog will crash if we use breadth-first search, but continue 
% to find a solution when we use iterative deepening:

% ?- solve(284, [2,50,4,4,8,7], Solution).  % using iterative deepening
% Solution = 4-(2-50+8)*7 
% Yes

% ?- solve(284, [2,50,4,4,8,7], Solution).  % using breadth-first search
% ERROR: Out of global stack

% Depth-first search works very well for this problem. In fact, it often 
% finds solutions faster than iterative deepening, albeit solutions that 
% tend to be longer (i.e., that involve more of the given numbers). 
% Indeed, iterative deepening (like breadth-first search) is designed  
% to always find the shortest solutions first (it is optimal), while 
% depth-first search is not. The fact that depth-first search is often 
% faster must be due to typical problem instances having many solutions. 
% Then it is unlikely that the shortest solution will be close to the left 
% margin of the search space, which means that an optimal search algorithm 
% (such as iterative deepening) typically has to visit a larger portion of 
% the search space than depth-first search before the first solution is 
% encountered.

% For this specific problem domain, there is no difference between simple 
% depth-first search and cycle-free depth-first search, because, due to 
% the way in which moves are defined, it is impossible to have a cycle on 
% a single branch. Each move reduces the number of terms in the list of 
% terms by one, so we cannot possibly re-visit the same state twice.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We are going to need our implementation of the A* algorithm:

:- consult('astar.pl').

% The following program can be used to find the shortest
% route between two cities in the Netherlands. We first give
% the program itself, which uses the A* algorithm, and then
% include the particular database of cities used to test the
% program and a couple of sample queries. At the end, there's
% a brief discussion on experiments with different heuristic
% functions and how these affect the runtime of the program.

% The top-level predicate is route/4. It takes two cities as
% the input and returns a route from a first to the second 
% as well as the distance to be travelled when following the
% proposed route. The predicate simply asserts the second
% city as the goal state (using the syntax used in the A*
% algorithm) and then uses A* to compute the solution.

route(Start, End, Route, Distance) :-
  set_target_city(End),
  solve_astar(Start, Route/Distance).

% The following predicate can be used to dynamically assert 
% a target city. This will be used as the goal state for A*.
% It first retracts any previously asserted facts regarding
% target_city/1 and then asserts the city provided. 

set_target_city(City) :-
  retractall(target_city(_)),
  assert(target_city(City)).

% The predicate goal/1 now called target_city/1 to check whether
% a given city is the target city. The reason we don't simply
% make goal/1 itself a dynamic predicate that can be used to
% assert the target city is that this would interfere with
% goal/1 used for Question 1.

goal(Goal) :-
  atom(Goal),
  target_city(Goal).

% This is our implementation of move/3, which is required by
% A*. The actual data of distances along roads between cities
% is given by the predicate street/2. As the distance between
% two cities will be the same in either direction, we use a
% disjunction (;) over the two ways of calling street/2. 

move(City1, City2, Distance) :-
  street(City1, City2, Distance) ; street(City2, City1, Distance).

% The predicate estimate/2 is also required by A*.
% It implements the heuristic function. In this case, we 
% use the straight-line distance between two cities as the
% estimate. This distance can be computed by comparing the
% x/y-coordinates of the two cities in question.

estimate(City, StraightLineDistance) :-
  target_city(Goal),
  coordinates(City, X1/Y1),
  coordinates(Goal, X2/Y2),
  distance(X1/Y1, X2/Y2, StraightLineDistance).

% The following is an auxiliary predicate to compute the
% straight-line distance between two coordinates on the grid. 

distance(X1/Y1, X2/Y2, Distance) :-
  Distance is sqrt((X1-X2)**2 + (Y1-Y2)**2).

% The predicates coordinates/2 is used to record the 
% X/Y coordinates of each city. Coordinates are given in
% kilometres with respect to the top lefthand corner of the
% map available at the following address (retrieved in 2005):
% http://www.infoplease.com/atlas/country/netherlands.html

coordinates(amsterdam,     125/150).
coordinates(arnhem,        195/190).
coordinates(breda,         115/235).
coordinates(eindhoven,     165/250).
coordinates(groningen,     240/055).
coordinates(gravenhage,    090/180).
coordinates(haarlem,       110/150).
coordinates(heerenveen,    195/085).
coordinates(hertogenbosch, 150/220).
coordinates(leeuwarden,    185/055).
coordinates(maastricht,    180/315).
coordinates(rotterdam,     100/200).
coordinates(utrecht,       140/180).
coordinates(witmarsum,     155/070).
coordinates(zwolle,        205/135).

% A better solution would be to compute the distance between
% two cities given their respective latitude and longitude.
% This is a bit more complicated, but has the advantage that
% it would be easier to build up a larger database of cities.

% The predicate street/2 is used to record the length of a
% connecting road between two given cities. Of course, this
% distance relation is symmetric, i.e., the distance between
% Amsterdam and 's-Gravenhage and that between 's-Gravenhage
% and Amsterdam are the same. Only one of them is listed
% here; the other direction is taken care of in the
% implementation of move/3. The actual distances were taken
% from http://locatienet.nl (in 2005).

street(amsterdam,     gravenhage,    58).
street(amsterdam,     haarlem,       20).
street(amsterdam,     heerenveen,   130).
street(amsterdam,     utrecht,       40).
street(amsterdam,     witmarsum,    110).
street(arnhem,        utrecht,       65).
street(arnhem,        zwolle,        68).
street(breda,         eindhoven,     61).
street(breda,         rotterdam,     49).
street(breda,         utrecht,       73).
street(eindhoven,     hertogenbosch, 37).
street(eindhoven,     maastricht,    87).
street(gravenhage,    haarlem,       57).
street(gravenhage,    rotterdam,     25).
street(groningen,     zwolle,       104).
street(groningen,     heerenveen,    58).
street(haarlem,       witmarsum,    120).
street(heerenveen,    leeuwarden,    34).
street(heerenveen,    witmarsum,     45).
street(heerenveen,    zwolle,        63).
street(hertogenbosch, utrecht,       64).
street(leeuwarden,    witmarsum,     41).
street(rotterdam,     utrecht,       60).
street(utrecht,       zwolle,        91).

% It does not matter whether you measure distances in, say,
% kilometres or miles, but it is important that you are using
% *the same* unit for both street distances and map coordinates!

% Sanity check:
% The data for the coordinates and the data for the street
% distances come from two different sources. This could be a
% problem, because the data may not be entirely consistent.
% I have used the following predicate to perform a "sanity
% check" of my data. The query check(X,Y) should fail. It can
% only succeed in case there is a pair of cities X and Y such
% that the street distance between them is less than their
% straight-line distance (= estimate). This would both be
% unrealistic and mean that the heuristic function is not
% admissible, because the estimate should always be less than
% or equal to the actual cost (i.e., A* may not compute the
% best solutions first, if this is not guaranteed). 

check(City1, City2) :-
  street(City1, City2, StreetDist),
  set_target_city(City2), % required for estimate/2 to work correctly
  estimate(City1, Estimate),
  StreetDist < Estimate, nl,
  write('True Distance:      '), write(StreetDist), nl,
  write('Estimated Distance: '), write(Estimate).

% Next we give the first three answers for two sample queries:

% ?- route(amsterdam, groningen, Route, Distance).
% Route = [amsterdam, heerenveen, groningen]
% Distance = 188 ;
% Route = [amsterdam, witmarsum, heerenveen, groningen]
% Distance = 213 ;
% Route = [amsterdam, utrecht, zwolle, groningen]
% Distance = 235
% Yes

% ?- route(breda, haarlem, Route, Distance).
% Route = [breda, rotterdam, gravenhage, haarlem]
% Distance = 131 ;
% Route = [breda, utrecht, amsterdam, haarlem]
% Distance = 133 ;
% Route = [breda, rotterdam, gravenhage, amsterdam, haarlem]
% Distance = 152
% Yes

% Experimentation with the trivial heuristic function h(n)=0:
% As discussed in the lecture, A* also works with a trivial
% heuristic function mapping any node n to 0, although it has
% also been said that this is a very inefficient variant of
% A*. To test this claim empirically, you can comment out the
% implementation of estimate/2 above and use the following
% line of code instead:

% estimate(_, 0).

% As you will see, the program still works and the best
% routes are still being returned first. The database used
% here is too small to notice any serious difference in
% performance either. However, using the time/1 predicate we
% can measure some changes. The following query has been run
% with the original program:

% ?- time(route(amsterdam, maastricht, Route, Distance)).
% 796 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
% Route = [amsterdam, utrecht, hertogenbosch, eindhoven, maastricht]
% Distance = 228
% Yes

% Then I have changed the code and used estimate(_,0) instead.
% This gives the following result:

% ?- time(route(amsterdam, maastricht, Route, Distance)).
% 11,413 inferences, 0.00 CPU in 0.01 seconds (0% CPU, Infinite Lips)
% Route = [amsterdam, utrecht, hertogenbosch, eindhoven, maastricht]
% Distance = 228
% Yes

% As you can see, there is in fact a significant increase in
% the amount of work that Prolog needs to do.

% As a final example, let us see what happens when we sharpen our
% heuristic function. Replace the original implementation of
% estimate/2 with the code below:

% estimate(City, Estimate) :-
%  target_city(Goal),
%  coordinates(City, X1/Y1),
%  coordinates(Goal, X2/Y2),
%  distance(X1/Y1, X2/Y2, StraightLineDistance),
%  Estimate is 1.4 * StraightLineDistance.

% That is, this new heuristic function adds 40% to the straight-line
% distance and uses that as an estimate. As a result, we should expect
% A* to get faster. And this is indeed the case (the number of Prolog
% inference steps reduces from 796 to 393):

% ?- time(route(amsterdam, maastricht, Route, Distance)).
% 393 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
% Route = [amsterdam, utrecht, hertogenbosch, eindhoven, maastricht],
% Distance = 228
% Yes

% But this increase in speed comes at a price. We can now not be certain
% anymore that A* will always return an optimal answer. However, these
% negative effects are still relatively minor. For example, if we try
% looking for a route from Amsterdam to Groningen again, the third solution
% returned is now not as good as it was originally (the algorithm now
% finds two routes of length 243km before it finds the route of 235km):

% ?- route(amsterdam, groningen, Route, Distance).
% Route = [amsterdam, heerenveen, groningen],
% Distance = 188 ;
% Route = [amsterdam, witmarsum, heerenveen, groningen],
% Distance = 213 ;
% Route = [amsterdam, witmarsum, leeuwarden, heerenveen, groningen],
% Distance = 243 ;
% Route = [amsterdam, haarlem, witmarsum, heerenveen, groningen],
% Distance = 243 ;
% Route = [amsterdam, utrecht, zwolle, groningen],
% Distance = 235
% Yes

% The reason for this suboptimal behaviour is that our heuristic
% is no longer admissible. We can check this using check/2, which
% will return all pairs of cities for which the true street distance
% is lower than the estimated distance. Example:

% ?- check(X, Y).
% True Distance:      58
% Estimated Distance: 64.5368
% X = amsterdam,
% Y = gravenhage
% Yes


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 3                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Let h_1, h_2 and h_3 be three admissible heuristic functions
% for A*. We can combine them into another heuristic function
% that is also admissible and that will guide the search at
% least as well as the best of the three given functions alone
% simply by constructing a function h that returns the maximum
% value of any of the three values returned by h_1, h_2, h_3.
% That is, h(n) = max{h_1(n),h_2(n),h_3(n)} for any given
% node n. The heuristic h is admissible, because the maximum
% of three functions that do not overestimate the real cost
% also does not overestimate the real cost. Furthermore, h is
% a heuristic that is at least as good as any of the original
% three heuristics, because it always returns an estimate that
% is at least as high as the estimate returned by any of the
% other three.

