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
  GameState = [CurrentBoard, white, PType1-StartPieces, PType2-StartPieces].

% display_game(+GameState)
% prints the game state to the terminal
display_game(GameState) :-
    GameState = [CurrentBoard,CurrentPlayer,PlayerTypeWhite-PiecesWhite,PlayerTypeBlack-PiecesBlack],
    nl,
    write('Blinq'),nl,
    write('--------------------'),nl,
    format('Current Player : ~s',CurrentPlayer), nl,
    format('White Type : ~s',PlayerTypeWhite), nl,
    format('White Pieces: ~d',PiecesWhite), nl,
    format('Black Type : ~s',PlayerTypeBlack), nl,
    format('Black Pieces: ~d',PiecesBlack), nl,
    write('--------------------'), nl,

    display_board(CurrentBoard).

% test_place()
% tests piece placement
test_place :-
  default(Element),
  create_board(Element,10,CurrentBoard),
  display_board(CurrentBoard),
  nl,
  place_piece(CurrentBoard,1-1,left,NewBoardLeft),
  display_board(NewBoardLeft),
  nl,
  place_piece(CurrentBoard,1-1,right,NewBoardRight),
  display_board(NewBoardRight),
  nl,
  place_piece(CurrentBoard,1-1,up,NewBoardUp),
  display_board(NewBoardUp),
  nl,
  place_piece(CurrentBoard,1-1,down,NewBoardDown),
  display_board(NewBoardDown).

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

% move(+GameState, +Move, -NewGameState)
% returns the new game state after a certain move, if the move is valid
move(GameState,X-Y,Orientation,NewGameState) :-
  GameState = [CurrentBoard,CurrentPlayer,PlayerTypeWhite-PiecesWhite,PlayerTypeBlack-PiecesBlack],
  place_piece(CurrentBoard,X-Y,Orientation,NewBoard),
  change_pieces(CurrentPlayer,PiecesWhite,PiecesBlack,NewPiecesWhite,NewPiecesBlack),
  switch_player(CurrentPlayer,NewPlayer),
  NewGameState = [NewBoard,NewPlayer,PlayerTypeWhite-NewPiecesWhite,PlayerTypeBlack-NewPiecesBlack].

test_move :-
  menu(GameConfig),
  initial_state(GameConfig,GameState),
  display_game(GameState),
  move(GameState,1-1,left,NewGameState),
  display_game(NewGameState).

% valid_moves(+GameState, -ListOfMoves)
% returns the list of possible moves in a certain game state
% case 1: piece is empty
% case 2: there is 2x2 plataform with the same Layer


% game_over(+GameState, -Winner)
% checks if the game is over in the current game state
% case 1: one of the players win (kinda dfs)
% case 2: one player loses all pieces (if the other player )
% case 3: both players lose all pieces (draw)
/* game_over(GameState, Winner) :-
  GameState = [CurrentBoard,CurrentPlayer,PlayerTypeWhite-PiecesWhite,PlayerTypeBlack-PiecesBlack], */
  


% value(+GameState, +Player, -Value)
% returns how good/bad is the current game state to player

% choose_move(+GameState, +Level, -Move)
% returns the move chosen by the computer player
% for human players, it interacts with the user to read the move