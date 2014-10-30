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
