%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logisch Programmeren en Zoektechnieken                    %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorting Algorithms: Bubblesort and Quicksort              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This file provides the implementations of naive bubblesort,
% improved bubblesort, and quicksort as introduced during the
% course. The code is identical to that given on the lecture
% slides. For explanations, please refer to the slides.

% For each of the three sorting algorithms, the first
% argument is the ordering relation to be used (such as <,
% for instance), the second argument is the input list, and
% the variable given in the third argument position will be
% instantiated with the sorted output list.

% Checking whether A and B are in the relation Rel:

check(Rel, A, B) :-
  Goal =.. [Rel,A,B],
  call(Goal).

% BUBBLESORT (naive version)
% Implementation of naive bubblesort (where we return to the
% front of the list after every successful swap):

bubblesort(Rel, List, SortedList) :-
  swap(Rel, List, NewList), !,
  bubblesort(Rel, NewList, SortedList).

bubblesort(_, SortedList, SortedList).

swap(Rel, [A,B|List], [B,A|List]) :-
  check(Rel, B, A).

swap(Rel, [A|List], [A|NewList]) :-
  swap(Rel, List, NewList).

% BUBBLESORT (improved version)
% Implementation of improved bubblesort (where we continue to
% swap wrongly ordered pairs until we reach the end of the
% list, before returning to the front again):

bubblesort2(Rel, List, SortedList) :-
  swap2(Rel, List, NewList),
  List \= NewList, !,
  bubblesort2(Rel, NewList, SortedList).

bubblesort2(_, SortedList, SortedList).

swap2(Rel, [A,B|List], [B|NewList]) :-
  check(Rel, B, A),
  swap2(Rel, [A|List], NewList).

swap2(Rel, [A|List], [A|NewList]) :-
  swap2(Rel, List, NewList).

swap2(_, [], []).

% QUICKSORT
% Implementation of quicksort:

quicksort(_, [], []).

quicksort(Rel, [Head|Tail], SortedList) :-
  split(Rel, Head, Tail, Left, Right),
  quicksort(Rel, Left, SortedLeft),
  quicksort(Rel, Right, SortedRight),
  append(SortedLeft, [Head|SortedRight], SortedList).

split(_, _, [], [], []).

split(Rel, Middle, [Head|Tail], [Head|Left], Right) :-
  check(Rel, Head, Middle), !,
  split(Rel, Middle, Tail, Left, Right).

split(Rel, Middle, [Head|Tail], Left, [Head|Right]) :-
  split(Rel, Middle, Tail, Left, Right).


