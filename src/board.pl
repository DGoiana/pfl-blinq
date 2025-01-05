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


char(black,9632).
char(white,9633).
char(empty,32).

big_char(black,9670).
big_char(white,9671).
big_char(empty,9974).


% display_item(+Item)
% prints an element
display_item(Item-_,X-Y,ValidMoves):- 
    \+ member(Y-X-_,ValidMoves),
    char(Item, C), 
    put_code(C), write(' ').

display_item(Item-_,X-Y,ValidMoves):- 
    member(Y-X-_,ValidMoves),
    big_char(Item, C), 
    put_code(C), write(' ').


% display_row(+List)
% display a row of the board
display_row(Row,CurrentLine,ValidMoves) :-
   display_row(Row,0,CurrentLine,ValidMoves).

display_row([],_,_,_).

display_row([Item|RemainingItems],CurrentElement,CurrentLine,ValidMoves):-
    pair_vl(CurrentElement),
    display_item(Item,CurrentLine-CurrentElement,ValidMoves),
    NewCurrentElement is CurrentElement+1,
    display_row(RemainingItems,NewCurrentElement,CurrentLine,ValidMoves).

display_row([Item|RemainingItems],CurrentElement,CurrentLine,ValidMoves):-
    \+ pair_vl(CurrentElement),
    display_item(Item,CurrentLine-CurrentElement,ValidMoves),
    NewCurrentElement is CurrentElement+1,
    display_row(RemainingItems,NewCurrentElement,CurrentLine,ValidMoves).


% display_board(+Board)
% display the whole board
display_board(Board,ValidMoves) :-
    length(Board,BoardSize),
    NewBoardSize is BoardSize*3+1,
    number_line(BoardSize), nl,
    write('y   '),hl(NewBoardSize,9633),nl,
    display_board(Board,NewBoardSize,0,ValidMoves),
    write('   '),put_code(9632),hl(NewBoardSize,45),put_code(9632),nl,
    write('    '),hl(NewBoardSize,9633),nl.

display_board([],_,_,_).

display_board([Row|RemainingRows],BoardSize,CurrentLine,ValidMoves):-
    pair_hl(CurrentLine,BoardSize),
    NewCurrentLine is CurrentLine+1,
    DisplayCurrentLine is (BoardSize//3 -1)-CurrentLine+1,
    write_number(DisplayCurrentLine), write(' '),put_code(9632),
    display_row(Row,CurrentLine,ValidMoves),
    write('|'), write(''),put_code(9632), nl,
    display_board(RemainingRows,BoardSize,NewCurrentLine,ValidMoves).

display_board([Row|RemainingRows],BoardSize,CurrentLine,ValidMoves):-
    \+ pair_hl(CurrentLine,BoardSize),
    NewCurrentLine is CurrentLine+1,
    DisplayCurrentLine is (BoardSize//3 -1)-CurrentLine+1,
    write_number(DisplayCurrentLine), write(' '),put_code(9632),
    display_row(Row,CurrentLine,ValidMoves),
    write('|'), write(''),put_code(9632), nl,
    display_board(RemainingRows,BoardSize,NewCurrentLine,ValidMoves).


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

% neutral
/* wb */
/* bw */
orientation(neutral,[white,black,black,white]).


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
