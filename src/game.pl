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

default(empty).

char(black,'b').
char(white,'w').
char(empty,'e').

% play()
% gives access to menu and starts game cycle
play :- 
  menu(GameConfig),
  initial_state(GameConfig,GameState).


% initial_state(+GameConfig, -GameState)
% returns the initial game state giving a game configuration
initial_state(GameConfig, GameState) :-
   default(Element),
   GameConfig = [BoardSize, PType1-PType2],
   create_board(Element, BoardSize, CurrentBoard),
   GameState = [CurrentBoard, white, PType1-54, PType2-54].

% display_game(+GameState)
% prints the game state to the terminal
display_game([Board,_,_,_]) :-
    display_board(Board).

run :-
    initial_state([5,bot-bot],GameState),
    display_game(GameState).

% move(+GameState, +Move, -NewGameState)
% returns the new game state after a certain move, if the move is valid


% valid_moves(+GameState, -ListOfMoves)
% returns the list of possible moves in a certain game state


% game_over(+GameState, -Winner)
% checks if the game is over in the current game state


% value(+GameState, +Player, -Value)
% returns how good/bad is the current game state to player


% choose_move(+GameState, +Level, -Move)
% returns the move chosen by the computer player
% for human players, it interacts with the user to read the move