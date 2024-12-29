between(Min, Max, Min):- Min =< Max.
between(Min, Max, Value):-
    Min < Max,
    NextMin is Min + 1,
    between(NextMin, Max, Value).

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

% hl(+BoardSize)
% displays a horizontal line of size BoardSize
hl(0).
hl(BoardSize) :-
    BoardSize > 0,
    write('-'),
    NewBoardSize is BoardSize-1,
    hl(NewBoardSize).

% pair_hl(+CurrentLine,+BoardSize)
% displays a horizontal line of size BoardSize if CurrentLine is even
pair_hl(CurrentLine,BoardSize) :-
    CurrentLine mod 2 =:= 0,
    write('  '),
    hl(BoardSize),nl.

% pair_vl(+CurrentLine,+BoardSize)
% displays a vertical line of size BoardSize if CurrentElement is even
pair_vl(CurrentElement) :-
    CurrentElement mod 2 =:= 0,
    write('| ').

convert_coords(X-Y,BoardSize,NewX-NewY) :-
  NewY is BoardSize-Y,
  NewX is X-1.

number_line(N) :-
  write('x '),
  number_line(N,N),
  write('|').
number_line(0,_).
number_line(N,Max) :-
  N > 0,
  N1 is N-1, 
  N2 is N-2,
  Current is Max-N1,
  Current2 is Max-N2,
  Current >= 10,
  Current2 >= 10,
  format('|~d ~d',[Current,Current2]),
  number_line(N2,Max).
number_line(N,Max) :-
  N > 0,
  N1 is N-1, 
  N2 is N-2,
  Current is Max-N1,
  Current2 is Max-N2,
  Current2 >= 10,
  format('| ~d ~d',[Current,Current2]),
  number_line(N2,Max).
number_line(N,Max) :-
  N > 0,
  N1 is N-1, 
  N2 is N-2,
  Current is Max-N1,
  Current2 is Max-N2,
  format('| ~d ~d ',[Current,Current2]),
  number_line(N2,Max).
/* number_line(N,Max) :-
  N > 0,
  N1 is N-1, 
  Current is Max-N1,
  Current >= 10,
  format('|~d',Current ),
  number_line(N1,Max). */