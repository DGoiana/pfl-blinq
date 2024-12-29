% main game file

% Appealing and intuitive 
% visualizations will be valued. Flexible  game state representations and visualization predicates will 
% also  be  valued,  for  instance  those  that  work  with  any    size

% If any configuration is required (beyond the standard installation of the software), or a font other 
% than the default one is used, this must be expressed in the README file, which must also include the steps 
% required to configure and/or install the necessary components (on Windows and Linux)

:- consult(board).
:- consult(utils).
:- consult(menu).

:- use_module(library(lists)).
:- use_module(library(random)).

default(empty-0).

char(black,'B').
char(white,'W').
char(empty,'e').

% play()
% gives access to menu and starts game cycle
play :- 
  menu(GameConfig),
  initial_state(GameConfig,GameState),
  game_loop(GameState).

% game_loop(+GameState)
% loop of the main game
game_loop(GameState) :-
  GameState = [_,CurrentPlayer,_,_,_],
  game_over(GameState,CurrentPlayer),
  show_winner(CurrentPlayer), !.
game_loop(GameState) :-
  display_game(GameState),
  choose_move(GameState,Move,Orientation),
  move(GameState,Move,Orientation,NewGameState),
  game_loop(NewGameState).

% initial_state(+GameConfig, -GameState)
% returns the initial game state giving a game configuration
initial_state(GameConfig, GameState) :-
  default(Element),
  GameConfig = [GameSize, PType1-PType2],
  BoardSize is GameSize*2, % each square needs to be 2x2 
  create_board(Element, BoardSize, CurrentBoard),
  get_pieces(GameSize,StartPieces),
  GameState = [CurrentBoard, white, PType1-StartPieces, PType2-StartPieces,0].

% display_game(+GameState)
% prints the game state to the terminal
display_game(GameState) :-
    GameState = [CurrentBoard,CurrentPlayer,PlayerTypeWhite-PiecesWhite,PlayerTypeBlack-PiecesBlack,MaxLayer],
    nl,
    write('Blinq'),nl,
    write('--------------------'),nl,
    format('Current Player : ~s',CurrentPlayer), nl,
    format('White Type : ~s',PlayerTypeWhite), nl,
    format('White Pieces: ~d',PiecesWhite), nl,
    format('Black Type : ~s',PlayerTypeBlack), nl,
    format('Black Pieces: ~d',PiecesBlack), nl,
    format('Max Layer: ~d',MaxLayer), nl,

    display_board(CurrentBoard).


% move(+GameState, +Move, -NewGameState)
% returns the new game state after a certain move, if the move is valid
move(GameState,X-Y,Orientation,NewGameState) :-
  GameState = [CurrentBoard,CurrentPlayer,PlayerTypeWhite-PiecesWhite,PlayerTypeBlack-PiecesBlack,MaxLayer],
  place_piece(CurrentBoard,X-Y,Orientation,NewBoard),
  change_pieces(CurrentPlayer,PiecesWhite,PiecesBlack,NewPiecesWhite,NewPiecesBlack),
  switch_player(CurrentPlayer,NewPlayer),
  get_piece(NewBoard,X-Y,Color-Layer),
  gt(Layer,MaxLayer,NewMaxLayer),
  NewGameState = [NewBoard,NewPlayer,PlayerTypeWhite-NewPiecesWhite,PlayerTypeBlack-NewPiecesBlack,NewMaxLayer].

check_valid_move(GameState,X-Y) :-
  valid_moves(GameState,ValidMoves),
  member(X-Y,ValidMoves).

% valid_moves(+GameState, -ListOfMoves)
% returns the list of possible moves in a certain game state
% case 1: piece is empty
% case 2: there is 2x2 plataform with the same Layer
valid_moves([Board,_,_,_,MaxLayer],ListOfMoves) :-
  length(Board,BoardLength),
  Max is BoardLength-2,
  findall(X-Y, (
    between(0, Max, X), X mod 2 =:= 0,
    between(0, Max, Y), Y mod 2 =:= 0,
    is_empty(Board, X-Y)
  ),EmptyMoves),
	NewMaxLayer is MaxLayer+1,
	get_plataforms(Board,BoardLength,NewMaxLayer,PlatformMoves),
	append(EmptyMoves,PlatformMoves,ListOfMoves).

% get_plataforms(+Board,+BoardSize,+Layer,-Moves)
% gets all availables moves atop 2x2 plataforms from MaxLayer to 1.
get_plataforms(_,_,1,[]).
get_plataforms(Board,BoardSize,Layer,Moves) :-
	Layer > 1,
	NewLayer is Layer-1,
	get_plataforms(Board,BoardSize,NewLayer,NewMoves),
	get_layer_plataform(Board,BoardSize,NewLayer,Result),
	append(NewMoves,Result,Moves).

% get_layer_plataform(+Board,+BoardSize,+Layer,-Moves)
% gets all available moves atop 2x2 plataforms in a given layer
get_layer_plataform(Board,BoardSize,Layer,Moves) :-
  findall(X-Y, (
    between(0, BoardSize, X), X mod 2 =:= 0,
    between(0, BoardSize, Y), Y mod 2 =:= 0,
		get_piece(Board,X-Y,_-Layer),
    check_plataform(Board, X-Y,Layer)
  ),Plataforms),
	maplist(plus_one,Plataforms,Moves).

% check_plataform(+Board,+Coords,+Layer)
% determines if there is a plataform starting on Coords
check_plataform(Board,X-Y,Layer) :-
	X2 is X+2,
	Y2 is Y+2,
	get_piece(Board,X-Y,_-Layer),
	get_piece(Board,X2-Y,_-Layer),
	get_piece(Board,X-Y2,_-Layer),
	get_piece(Board,X2-Y2,_-Layer).


result(-1, neutral).
result(0, draw).
result(1, white).
result(2, black).

% game_over(+GameState, -Winner)
% checks if the game is over in the current game state
% case 1: one of the players win (kinda dfs)
% case 2: one player loses all pieces (if the other player )
% case 3: both players lose all pieces (draw)
game_over([Board,_,_,_,_],white) :-
  check_winner(Board,white).
game_over([Board,_,_,_,_],black) :-
  check_winner(Board,black).

% check_winner(+Board, -Winner)
% checks if anyone has won the game in the current board
check_winner(Board, Player) :- 
  dfs(Board, Player, Winner),
  Player = Winner.

% dfs(+Board, +Player, -Won)
% performs a dfs to see if the player has already won the game
dfs(Board, white, Won) :-
  findall(0-Y, (nth0(0, Board, Row), (nth0(Y, Row, white-_))), Pieces),
  member(Start, Pieces),
  dfs_visit(Board, white, [Start], [], Won).

dfs(Board, black, Won) :-
  findall(X-0, (nth0(X, Board, Row), (nth0(0, Row, black-_))), Pieces),
  member(Start, Pieces),
  dfs_visit(Board, black, [Start], [], Won).
  
% dfs_visit(+Board, +Player, +ToVisit, +AlreadyVisited, -Won)
% helper function for dfs traversal
dfs_visit(_, _, [], _, false).
dfs_visit(Board, Player, [Current|_], _, Player) :-
  Current = X-Y,
  end_game(Board, X-Y, Player).
dfs_visit(Board, Player, [Current|ToVisit], Visited, Won) :-
  Current = X-Y,
  findall(Next, (neighbor(X-Y, Next), within_coords(Board, Next), \+ member(Next, Visited), \+ member(Next, ToVisit), check_color(Board, Current, Next)), Neighbors),
  append(Neighbors, ToVisit, NewToVisit),
  sort(NewToVisit, SortedToVisit),
  dfs_visit(Board, Player, SortedToVisit, [Current|Visited], Won).

% neighbor(+Coords, -NeighborCoords)
% return neighbor coords
neighbor(X-Y, X1-Y) :- X1 is X + 1.
neighbor(X-Y, X1-Y) :- X1 is X - 1.
neighbor(X-Y, X-Y1) :- Y1 is Y + 1.
neighbor(X-Y, X-Y1) :- Y1 is Y - 1.

% end_game(+Coords, +Player)
% check if any player has a win condition
end_game(Board, Coords, white) :-
  Coords = X-_,
  length(Board, Size),
  X is Size - 1.

end_game(Board, Coords, black) :-
  Coords = _-Y,
  length(Board, Size),
  Y is Size - 1.

% within_coords(+Board, +Coords)
% check if move is within valid coords
within_coords(Board, X-Y) :-
  length(Board, Size),
  NewSize is Size - 1,
  between(0, NewSize, X),
  between(0, NewSize, Y).

% check_color(+Board, +CoordsCurrent, +CoordsNext)
% check if the color from the next neigbor is the same as the current
check_color(Board, X1-Y1, X2-Y2) :-
  nth0(X1, Board, Row1),
  nth0(Y1, Row1, Color1-_),
  nth0(X2, Board, Row2),
  nth0(Y2, Row2, Color2-_),
  Color1 = Color2.
  

% value(+GameState, +Player, -Value)
% returns how good/bad is the current game state to player

% choose_move(+GameState, +Level, -Move)
% returns the move chosen by the computer player
% for human players, it interacts with the user to read the move
/*
choose_move([Board,white,hardBot-WhitePieces,BlackType-BlackPieces,MaxLayer],X-Y) :- .
*/
choose_move([Board,white,easyBot-_,_-_,MaxLayer],X-Y,Orientation) :- 
  PossibleOrientations = [left,right,up,down],
  valid_moves([Board,_,_,_,MaxLayer],Moves),
  random_member(Orientation,PossibleOrientations),
  random_member(X-Y, Moves).
choose_move([Board,white,player-_,_-_,MaxLayer],X-Y,Orientation) :-
  /* TODO: Convert to convention: (1,1) at bottom left corner */
  nl,
  write('White to move'), nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('--------------------'),nl,

  valid_moves([Board,_,_,_,MaxLayer],Moves),
  write(Moves), nl,

  length(Board,N),
  Max is N-1,
  repeat,
  write('X Coord:'),
  get_input(0,Max,X),
  write('Y Coord:'),
  get_input(0,Max,Y),
  get_orientation(Orientation),
  check_valid_move([Board,_,_,_,MaxLayer],X-Y),
  !.
/* 
choose_move([Board,black,WhiteType-WhitePieces,hardBot-BlackPieces,MaxLayer],X-Y) :- .
*/
choose_move([Board,black,_-_,easyBot-_,MaxLayer],X-Y,Orientation) :- 
  PossibleOrientations = [left,right,up,down],
  valid_moves([Board,_,_,_,MaxLayer],Moves),
  random_member(Orientation,PossibleOrientations),
  random_member(X-Y, Moves).
choose_move([Board,black,_-_,player-_,MaxLayer],X-Y,Orientation) :- 
  /* TODO: Convert to convention: (1,1) at bottom left corner */
  nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('Black to move'), nl,
  write('--------------------'),nl,

  valid_moves([Board,_,_,_,MaxLayer],Moves),
  write(Moves), nl,

  length(Board,N),
  Max is N-1,
  repeat,
  write('X Coord:'),
  get_input(0,Max,X),
  write('Y Coord:'),
  get_input(0,Max,Y),
  get_orientation(Orientation),
  check_valid_move([Board,_,_,_,MaxLayer],X-Y),
  !.