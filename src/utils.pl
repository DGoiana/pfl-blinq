% between(+Min,+Max,?Value)
% determines if a number is between range
between(Min,Max,Value) :-
  Value @>= Min,
  Value @=< Max.

% get_input(+Min,+Max,-Value)
% gets an input value from user until the value is in range (Min,Max)
get_input(Min,Max,Value) :-
  repeat,
  read(Value),
  number(Value),
  between(Min,Max,Value), !.

% get_pieces(+BoardSize,-Pieces)
% get number of starting pieces from BoardSize
get_pieces(5,27).
get_pieces(BoardSize,Pieces) :-
  BoardSize =\= 5,
  Pieces is BoardSize * 5.


% replace(+Index,+List,+Value,-NewList)
% replaces a list element with another
replace(Index,L1,Elem,L2) :- replace(Index,L1,Elem,L2,[]).
replace(0,[_| T],Elem, L2,Result) :-
  append(Result,[Elem | T],L2).
replace(Index,[H | T],Elem,L2,Result) :-
  NewIndex is Index-1,
  append(Result,[H],NewResult),
  replace(NewIndex,T,Elem,L2,NewResult).

% nth(+Index,+List,-Elem)
% gets the nth element of a list
nth(N,List,Elem) :- 
  length(Prefix,N),
  append(Prefix,[Elem | _],List).