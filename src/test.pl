:- consult(game).

test_board :-
  GameConfig = [5,player-player],
  initial_state(GameConfig,GameState),
  display_game(GameState), !.
test_board(Size) :-
  GameConfig = [Size,player-player],
  initial_state(GameConfig,GameState),
  display_game(GameState), !.


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

test_move :-
  GameConfig = [5,player-player],
  initial_state(GameConfig,GameState),
  display_game(GameState),
  move(GameState,2-2,down,NewGameState),
  display_game(NewGameState).
/* 
  display_game(NewGameState),
  move(NewGameState,4-2,left,NewGameState2),
  display_game(NewGameState2).
 */
test_empty_board_plataform :-
  GameConfig = [5,player-player],
  initial_state(GameConfig,GameState),
	display_game(GameState),
	valid_moves(GameState,Moves),!.

test_plataform :-
  GameConfig = [5,player-player],
  initial_state(GameConfig,GameState),
  move(GameState,    0-0-left,NewGameState1),
  move(NewGameState1,2-0-left,NewGameState2),
  move(NewGameState2,0-2-left,NewGameState3),
  move(NewGameState3,2-2-left,NewGameState4),

/*   move(NewGameState4,4-4-left,NewGameState5),
  move(NewGameState5,6-4-left,NewGameState6),
  move(NewGameState6,4-6-left,NewGameState7),
  move(NewGameState7,6-6-left,NewGameState8), */

  move(NewGameState4,6-4-left,NewGameState5),
  move(NewGameState5,4-6-left,NewGameState6),
  move(NewGameState6,6-6-left,NewGameState7),

/* 	display_game(NewGameState8),
	valid_moves(NewGameState8,Moves),!. */
	display_game(NewGameState7),
	valid_moves(NewGameState7,Moves),!.
	% write(Moves).

test_valid_moves :-
  GameConfig = [5,player-player],
  initial_state(GameConfig,GameState),
  display_game(GameState),
  valid_moves(GameState,Moves),
	write(Moves),!.

test_choose_move :-
  GameConfig = [5,player-player],
  initial_state(GameConfig,GameState),
  display_game(GameState),
  choose_move(GameState,_,Move),
  move(GameState,Move,NewGameState),
  display_game(NewGameState),!.
  %test_choose_move.

test_random_move :-
  GameConfig = [5,easyBot-player],
  initial_state(GameConfig,GameState),
  choose_move(GameState,_,Move),
  move(GameState,Move,NewGameState),
  display_game(NewGameState),!.

test_batch :-
  test_random_move,
  test_random_move,
  test_random_move,
  test_random_move.

test_easy_bot :-
  GameConfig = [5,easyBot-easyBot],
  initial_state(GameConfig,GameState),
  game_loop(GameState),!.

test_hard_bot :-
  GameConfig = [5,hardBot-hardBot],
  initial_state(GameConfig,GameState),
  game_loop(GameState),!.

test_player :-
  GameConfig = [3,player-player],
  initial_state(GameConfig,GameState),
  game_loop(GameState).