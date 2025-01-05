%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\

max(A,B,C) :-
  max_member(C,[A,B]).

between(Min, Max, Min):- Min =< Max.
between(Min, Max, Value):-
    Min < Max,
    NextMin is Min + 1,
    between(NextMin, Max, Value).


read_number(X) :- read_number(X,0).
read_number(Acc,Acc) :- peek_code(10),get_code(10),!.
read_number(X,Acc) :-
  get_code(V),
  Number is V-48,
  NewAcc is Acc*10 + Number,
  read_number(X,NewAcc).

% get_input(+Min,+Max,-Value)
% gets an input value from user until the value is in range (Min,Max)
get_input(Min,Max,Value) :-
  repeat,
  read_number(Value),
  validate_input(Min,Max,Value), !.

validate_input(Min,Max,Value) :-
  between(Min,Max,Value).

validate_input(Min,Max,Value) :-
  \+ between(Min,Max,Value),
  write('Invalid Input'), nl,
  write('Try again:'),
  fail.

% get_pieces(+BoardSize,-Pieces)
% get number of starting pieces for each player from BoardSize
get_pieces(BoardSize,Pieces) :-
  calculate_pieces(BoardSize,TotalPieces),
  Pieces is round(TotalPieces) // 2.

calculate_pieces(0,0).
calculate_pieces(N,Sum) :-
  N > 0,
  NewN is N-1,
  calculate_pieces(NewN,Result),
  Sum is Result+(N**2).

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
hl(0,_).
hl(BoardSize,Symbol) :-
    BoardSize > 0,
    put_code(Symbol),
    NewBoardSize is BoardSize-1,
    hl(NewBoardSize,Symbol).

% pair_hl(+CurrentLine,+BoardSize)
% displays a horizontal line of size BoardSize if CurrentLine is even
pair_hl(CurrentLine,BoardSize) :-
    CurrentLine mod 2 =:= 0,
    write('   '),put_code(9632),
    hl(BoardSize,45),put_code(9632),nl.

% pair_vl(+CurrentLine,+BoardSize)
% displays a vertical line of size BoardSize if CurrentElement is even
pair_vl(CurrentElement) :-
    CurrentElement mod 2 =:= 0,
    write('| ').

% convert_coords(+X-Y,+BoardSize,-X-Y)
% Converts the coordinates from (0,0) on the left upper corner as the first to (1,1) on the left down corner
convert_coords(X-Y,BoardSize,NewX-NewY) :-
  NewY is BoardSize-Y,
  NewX is X-1.

% number_line(+N)
% Prints a number line from 1-N
number_line(N) :-
  write('  x '),
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

% show_winner(+Winner)
% Shows the winner
show_winner(black) :-
  nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('Black Won'), nl,
  write('--------------------'),nl.
show_winner(white) :-
  nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('White Won'), nl,
  write('--------------------'),nl.
show_winner(draw) :-
  nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('Draw'), nl,
  write('--------------------'),nl.

% get_middle(+BoardSize,-X-Y)
% gets the coordinates on the middle of the board
get_middle(BoardSize,X-Y) :-
  X is BoardSize // 2 - 1,
  Y is BoardSize // 2 - 1.

% write_number(+N)
% writes a number with a padding
write_number(N) :-
    N < 10,
    write(N),write(' ').
write_number(N) :-
    N >= 10,
    write(N).