% main game file

% Appealing and intuitive 
% visualizations will be valued. Flexible  game state representations and visualization predicates will 
% also  be  valued,  for  instance  those  that  work  with  any  board  size

% If any configuration is required (beyond the standard installation of the software), or a font other 
% than the default one is used, this must be expressed in the README file, which must also include the steps 
% required to configure and/or install the necessary components (on Windows and Linux)

:- consult(board).
:- consult(utils).
:- consult(menu).

default(empty-0).

char(black,'b').
char(white,'w').
char(empty,'e').

% play()
% gives access to menu and starts game cycle
play(GameState) :- 
  menu(GameConfig),
  initial_state(GameConfig,GameState).

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
    write('--------------------'), nl,

    display_board(CurrentBoard).


% move(+GameState, +Move, -NewGameState)
% returns the new game state after a certain move, if the move is valid
move(GameState,X-Y,Orientation,NewGameState) :-
  GameState = [CurrentBoard,CurrentPlayer,PlayerTypeWhite-PiecesWhite,PlayerTypeBlack-PiecesBlack,MaxLayer],
  place_piece(CurrentBoard,X-Y,Orientation,NewBoard),
  change_pieces(CurrentPlayer,PiecesWhite,PiecesBlack,NewPiecesWhite,NewPiecesBlack),
  switch_player(CurrentPlayer,NewPlayer),
  get_piece(CurrentBoard,X-Y,_-Layer),
  gt(Layer,MaxLayer,NewMaxLayer),
  NewGameState = [NewBoard,NewPlayer,PlayerTypeWhite-NewPiecesWhite,PlayerTypeBlack-NewPiecesBlack,NewMaxLayer].

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

% game_over(+GameState, -Winner)
% checks if the game is over in the current game state
% case 1: one of the players win (kinda dfs)
% case 2: one player loses all pieces (if the other player )
% case 3: both players lose all pieces (draw)
  


% value(+GameState, +Player, -Value)
% returns how good/bad is the current game state to player

% choose_move(+GameState, +Level, -Move)
% returns the move chosen by the computer player
% for human players, it interacts with the user to read the move