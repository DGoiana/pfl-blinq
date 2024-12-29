% BOARD

% create_list(+Element,+Size,-List)
% creates a list with an Element repeated Size times
create_list(_, 0, []).
create_list(Element, Size, [Element|Sublist]):-
    Size > 0,
    Size1 is Size - 1,
    create_list(Element, Size1, Sublist).

% create_board(+Element,+Size,-Board)
% creates a list with size Size and then creates a list of lists with size Size
create_board(Element, Size, Board):-
    create_list(Element, Size, List),
    create_list(List, Size, Board).

% DISPLAY BOARD

% display_item(+Item)
% prints an element
display_item(Item-Layer):- 
    char(Item, C), 
    write(C), write(' ').

% display_row(+List)
% display a row of the board
display_row(Row) :-
   display_row(Row,0).

display_row([],_).
display_row([Item|RemainingItems],CurrentElement):-
    pair_vl(CurrentElement),
    display_item(Item),
    NewCurrentElement is CurrentElement+1,
    display_row(RemainingItems,NewCurrentElement).
display_row([Item|RemainingItems],CurrentElement):-
    \+ pair_vl(CurrentElement),
    display_item(Item),
    NewCurrentElement is CurrentElement+1,
    display_row(RemainingItems,NewCurrentElement).

% display_board(+Board)
% display the whole board
display_board(Board) :-
    length(Board,BoardSize),
    NewBoardSize is BoardSize*3+1,
    display_board(Board,NewBoardSize,0),
    hl(NewBoardSize),nl.

display_board([],_,_).
display_board([Row|RemainingRows],BoardSize,CurrentLine):-
    pair_hl(CurrentLine,BoardSize),
    display_row(Row),
    write('|'), nl,
    NewCurrentLine is CurrentLine+1,
    display_board(RemainingRows,BoardSize,NewCurrentLine).
display_board([Row|RemainingRows],BoardSize,CurrentLine):-
    \+ pair_hl(CurrentLine,BoardSize),
    display_row(Row),
    write('|'), nl,
    NewCurrentLine is CurrentLine+1,
    display_board(RemainingRows,BoardSize,NewCurrentLine).

% left
/* bw */
/* bw */
orientation(left,[black,white,black,white]).

% right
/* wb */
/* wb */
orientation(right,[white,black,white,black]).

% up
/* bb */
/* ww */
orientation(up,[black,black,white,white]).

% down
/* ww */
/* bb */
orientation(down,[white,white,black,black]).

% place_piece(+LastBoard,+Coords,+Orientation,-NewBoard)
% puts a given piece (based on its orientation) in the board
place_piece(LastBoard,X-Y,Orientation,NewBoard) :-
  X1 is X+1,
  Y1 is Y+1,
  orientation(Orientation,Colors),
  Colors = [C1,C2,C3,C4],
  place_square(LastBoard,X-Y,C1,NextBoard),
  place_square(NextBoard,X1-Y,C2,NextNextBoard),
  place_square(NextNextBoard,X-Y1,C3,NextNextNextBoard),
  place_square(NextNextNextBoard,X1-Y1,C4,NewBoard).

% place_square(+LastBoard,+Coords,+NewValue,-NewBoard)
% places a value in a square on the board
place_square(LastBoard,XCoord-YCoord,Color,NewBoard) :-
  nth(YCoord,LastBoard,Line),
  nth(XCoord,Line,_-Layer),
  NewLayer is Layer+1,
  replace(XCoord,Line,Color-NewLayer,NewLine),
  replace(YCoord,LastBoard,NewLine,NewBoard).