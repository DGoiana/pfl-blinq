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
    write(C-Layer), write(' ').

% display_row(+List)
% display a row of the board
display_row([]).
display_row([Item|RemainingItems]):-
    display_item(Item),
    display_row(RemainingItems).

% display_board(+Board)
% display the whole board
display_board([]).
display_board([Row|RemainingRows]):-
    display_row(Row), nl,
    display_board(RemainingRows).

% left
/* bw */
/* bw */
orientation(left,[black,white,black,white]).

% right
/* wb */
/* wb */
orientation(right,[white,black,white,black]).

% right
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
  orientation(Orientation,[C1,C2,C3,C4]),
  place_square(LastBoard,X-Y,C1,NextBoard),
  place_square(NextBoard,X-Y1,C2,NextNextBoard),
  place_square(NextNextBoard,X1-Y,C3,NextNextNextBoard),
  place_square(NextNextNextBoard,X1-Y1,C4,NewBoard).

% place_square(+LastBoard,+Coords,+NewValue,-NewBoard)
% places a value in a square on the board
place_square(LastBoard,XCoord-YCoord,Color,NewBoard) :-
  nth(XCoord,LastBoard,Line),
  nth(YCoord,Line,_-Layer),
  NewLayer is Layer+1,
  replace(YCoord,Line,Color-NewLayer,NewLine),
  replace(XCoord,LastBoard,NewLine,NewBoard).