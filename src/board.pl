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
display_item(Item):- 
    char(Item, C), 
    write(C).

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
