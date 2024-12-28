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

% get_piece(+Board,+Coords,-Piece)
% gets a piece from its coordinates
get_piece(Board,X-Y,Piece) :-
  nth(Y,Board,Line),
  nth(X,Line,Piece).

% is_empty(+Board,+Coord)
% checks if piece is empty
is_empty(Board,X-Y) :-
  get_piece(Board,X-Y,empty-0).

% gt(+X,+Y,-Z)
% returns the maximum between two values
gt(X,Y,Z) :- Z is max(X,Y) .

% plus_one(+Coords,-NewCoords)
% adds an offset of 1 to coordinates
plus_one(X-Y,NewX-NewY) :-
	NewX is X+1,
	NewY is Y+1.

% maplist(+Predicate,+List,-Result)
% Applies the Predicate to all elements of List
maplist(_, [], []).
maplist(Pred, [X|Xs], [Y|Ys]) :-
    call(Pred, X, Y), 
    maplist(Pred, Xs, Ys).

% switch_player(+CurrentPlayer,-NewPlayer)
% changes players
switch_player(black,white).
switch_player(white,black).

% change_pieces(+CurrentPlayer,+CurrentWhitePieces,+CurrentBlackPieces,-NewWhitePieces,-NewBlackPieces)
% decreases the number of pieces according to player
change_pieces(white,CurrentWhitePieces,CurrentBlackPieces,NewWhitePieces,CurrentBlackPieces) :-
  NewWhitePieces is CurrentWhitePieces-1.
change_pieces(black,CurrentWhitePieces,CurrentBlackPieces,CurrentWhitePieces,NewBlackPieces) :-
  NewBlackPieces is CurrentBlackPieces-1.