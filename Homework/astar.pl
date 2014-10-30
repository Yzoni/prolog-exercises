%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logisch Programmeren en Zoektechnieken                    %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Best-first Search with the A* Algorithm                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This file provides an implementation of the A* algorithm.
% Additional explanations may also be found on the lecture
% slides.

% Problem specification:
% The algorithm assumes that the problem domain has been
% specified by the user by implementing the predicates
% move/3, goal/1 and estimate/1. See lecture slides for an
% explanations of what these predicates do. 

% The predicate solve_astar/2 provides the "user interface"
% of the algorithm. This should be used to initiate a search.
% The first argument is the initial node and the variable
% given in the second argument position will be instantiated
% with a solution path labelled with the associated cost. 

solve_astar(Node, Path/Cost) :-
  estimate(Node, Estimate),
  astar([[Node]/0/Estimate], RevPath/Cost/_),
  reverse(RevPath, Path).

% The predicate astar/2 implements the actual algorithm
% itself. The first argument is the list of paths labelled
% with current cost and current estimate to a goal node and
% the second argument will eventually be instantiated with 
% a labelled solution path. In case the best of the current
% path ends in a goal node, we succeed. Otherwise, we extract
% the best path from the list of paths, generate all its
% expansions, and continue with the union of the remaining
% and the expanded paths. 

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

% The predicate get_best/2 implements the search strategy  
% used in the A* algorithm. It takes a list of labelled paths
% and returns the path that is considered best, i.e., that
% minimises the sum of the current cost and the estimated
% cost of reaching a goal node. The predicate works by going
% recursively through the list of labelled paths, inspecting
% the first two elements of the list in each round and
% keeping the one with the lower estimated overall cost,
% until only a single labelled path is left. Changing the
% implementation of this predicate alone would be enough to
% implement any other best-first search algorithm. 

get_best([Path], Path) :- !.  

get_best([Path1/Cost1/Est1,_/Cost2/Est2|Paths], BestPath) :-
  Cost1 + Est1 =< Cost2 + Est2, !,
  get_best([Path1/Cost1/Est1|Paths], BestPath).

get_best([_|Paths], BestPath) :-
  get_best(Paths, BestPath).

% The predicate expand_astar/2 generates all the paths we get
% by making a single additional move from the end node of the
% input path. 

expand_astar(Path, ExpPaths) :-
  findall(NewPath, move_astar(Path,NewPath), ExpPaths).

% The predicate move_astar/2 provides a wrapper around the
% user-defined move/3. It takes a labelled path as an input
% and generates a possible follow-up path, also labelled 
% with the current cost and the appropriate estimate. The
% predicate also checks for cycles. 

move_astar([Node|Path]/Cost/_, [NextNode,Node|Path]/NewCost/Estimate) :-
  move(Node, NextNode, StepCost),
  \+ member(NextNode, Path),
  NewCost is Cost + StepCost,
  estimate(NextNode, Estimate).
