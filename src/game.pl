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


% play()
% gives access to menu and starts game cycle
play :- 
  menu(GameConfig),
  initial_state(GameConfig,GameState),
  game_loop(GameState, 1).

% game_loop(+GameState)
% loop of the main game
game_loop([Board,_,_-0,_-_,_,_], _) :-
  show_winner(draw),
  display_board(Board,[]), !.
game_loop([Board,_,_-_,_-0,_,_], _) :-
  show_winner(draw),
  display_board(Board,[]), !.
game_loop(GameState, _) :-
  GameState = [Board,_,_,_,_,_],
  game_over(GameState,white),
  show_winner(white),
  display_board(Board,[]), !.
game_loop(GameState, _) :-
  GameState = [Board,_,_,_,_,_],
  game_over(GameState,black),
  show_winner(black),
  display_board(Board,[]), !.
game_loop(GameState, Play) :-
  display_game(GameState),
  choose_move(GameState, Move, Play),
  move(GameState,Move,NewGameState),
  NextPlay is Play+1,
  game_loop(NewGameState, NextPlay).

% initial_state(+GameConfig, -GameState)
% returns the initial game state giving a game configuration
initial_state(GameConfig, GameState) :-
  default(Element),
  GameConfig = [GameSize, PType1-PType2],
  BoardSize is GameSize*2, % each square needs to be 2x2 
  create_board(Element, BoardSize, CurrentBoard),
  get_middle(BoardSize,X-Y),
  place_piece(CurrentBoard,X-Y,neutral,NewBoard),
  get_pieces(GameSize,StartPieces),
  GameState = [NewBoard, white, PType1-StartPieces, PType2-StartPieces,0,0-0].

% display_game(+GameState)
% prints the game state to the terminal
display_game(GameState) :-
    GameState = [CurrentBoard,CurrentPlayer,PlayerTypeWhite-PiecesWhite,PlayerTypeBlack-PiecesBlack,MaxLayer,LongestSequenceWhite-LongestSequenceBlack],
    nl,
    write('Blinq'),nl,
    write('--------------------'),nl,
    format('Current Player : ~s',CurrentPlayer), nl,
    format('White Type : ~s',PlayerTypeWhite), nl,
    format('White Pieces: ~d',PiecesWhite), nl,
    format('Black Type : ~s',PlayerTypeBlack), nl,
    format('Black Pieces: ~d',PiecesBlack), nl,
    format('Max Layer: ~d',MaxLayer), nl,
    format('LongestSequenceWhite: ~d',LongestSequenceWhite), nl,
    format('LongestSequenceBlack: ~d',LongestSequenceBlack), nl,
    write('--------------------'),nl,
    nl,

    valid_moves(GameState,ValidMoves),
    display_board(CurrentBoard,ValidMoves).


% move(+GameState, +Move, -NewGameState)
% returns the new game state after a certain move, if the move is valid
move(GameState,X-Y-Orientation,NewGameState) :-
  GameState = [CurrentBoard,CurrentPlayer,PlayerTypeWhite-PiecesWhite,PlayerTypeBlack-PiecesBlack,MaxLayer,LongestSequenceWhite-LongestSequenceBlack],
  place_piece(CurrentBoard,X-Y,Orientation,NewBoard),
  change_pieces(CurrentPlayer,PiecesWhite,PiecesBlack,NewPiecesWhite,NewPiecesBlack),
  switch_player(CurrentPlayer,NewPlayer),
  get_piece(NewBoard,X-Y,_-Layer),
  gt(Layer,MaxLayer,NewMaxLayer),
  NewGameState = [NewBoard,NewPlayer,PlayerTypeWhite-NewPiecesWhite,PlayerTypeBlack-NewPiecesBlack,NewMaxLayer,LongestSequenceWhite-LongestSequenceBlack].

check_valid_move(X-Y-_,ValidMoves) :-
  member(X-Y-_,ValidMoves).

% valid_moves(+GameState, -ListOfMoves)
% returns the list of possible moves in a certain game state
% case 1: piece is empty
% case 2: there is 2x2 plataform with the same Layer
valid_moves([Board,_,_,_,MaxLayer,_],ListOfMoves) :-
  length(Board,BoardLength),
	NewMaxLayer is MaxLayer+1,
	get_platforms(Board,BoardLength,NewMaxLayer,ListOfMoves).
  
% get_platforms(+Board,+BoardSize,+Layer,-Moves)
% gets all availables moves atop 2x2 platforms from MaxLayer to 1.
get_platforms(_,_,0,[]).
get_platforms(Board,BoardSize,Layer,Moves) :-
	Layer > 0,
	NewLayer is Layer-1,
	get_platforms(Board,BoardSize,NewLayer,NewMoves),
	get_layer_plataform(Board,BoardSize,NewLayer,Result),
	append(NewMoves,Result,Moves).

% get_layer_plataform(+Board,+BoardSize,+Layer,-Moves)
% gets all available moves atop 2x2 platforms in a given layer
get_layer_plataform(Board,BoardSize,Layer,Moves) :-
  Layer mod 2 =:= 0,
  findall(X-Y-Orientation, (
    between(0, BoardSize, X), X mod 2 =:= 0,
    between(0, BoardSize, Y), Y mod 2 =:= 0,
    check_layer(Board,X-Y-Orientation,Layer)
  ),Moves).
get_layer_plataform(Board,BoardSize,Layer,Moves) :-
  Layer mod 2 =:= 1,
  findall(X-Y-Orientation, (
    between(0, BoardSize, X), X mod 2 =:= 1,
    between(0, BoardSize, Y), Y mod 2 =:= 1,
    check_layer(Board,X-Y-Orientation,Layer)
  ),Moves).

check_layer(Board,X-Y-Orientation,Layer) :-
  get_piece(Board,X-Y,_-Layer),
  check_plataform(Board, X-Y,Layer),
  member(Orientation, [left, right, up, down]).

% check_plataform(+Board,+Coords,+Layer)
% determines if there is a plataform starting on Coords
check_plataform(Board,X-Y,Layer) :-
	X2 is X+1,
	Y2 is Y+1,
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
game_over([Board,_,_,_,_,_],white) :- 
  white_wins(Board), !.
game_over([Board,_,_,_,_,_],black) :- 
  black_wins(Board), !.
game_over(_,draw).

% black_wins(+Board)
% checks if black has a winning board
black_wins(Board) :-
  length(Board, BoardSize),
  findall(0-Y, (nth0(Y, Board, Row), nth0(0, Row, black-_)), Pieces),
  length(Pieces, N), N > 0,
  Last is BoardSize - 1,
  member(Piece, Pieces),
  path_exists(Board, Piece, black, Last-_).


% checks if white has a winning board
white_wins(Board) :-
  length(Board, BoardSize),
  findall(X-0, (nth0(0, Board, Row), nth0(X, Row, white-_)), Pieces),
  length(Pieces, N), N > 0,
  Last is BoardSize - 1,
  member(Piece, Pieces),
  path_exists(Board, Piece, white, _-Last).

% path_exists(+Board, +Start, +Color, +Target)
% checks if there is a path between start and target
path_exists(Board, Start, Color, Target) :-
  dfs(Board, [Start], [], Color, Target).

% dfs(+Board, +Stack, +Visited, +Color, -End, +TargetRowOrCol)
% found a path from start to target
dfs(_, [Current|_], _, _, Target) :-
  Current = Target.

% performs dfs
dfs(Board, [Current|Rest], Visited, Color, Target) :-
  Current \= Target,
  findall(Neighbor, (
    neighbor(Current, Neighbor),
    within_coords(Board, Neighbor),
    check_color(Board, Neighbor, Color),
    \+ member(Neighbor, Rest),
    \+ member(Neighbor, Visited)
  ), Neighbors),
  append(Neighbors, Rest, NewStack),
  dfs(Board, NewStack, [Current|Visited], Color, Target).

% neighbor(+Coords, -NeighborCoords)
% return neighbor coords
neighbor(X-Y, X1-Y) :- X1 is X + 1.
neighbor(X-Y, X1-Y) :- X1 is X - 1.
neighbor(X-Y, X-Y1) :- Y1 is Y + 1.
neighbor(X-Y, X-Y1) :- Y1 is Y - 1.


% within_coords(+Board, +Coords)
% check if move is within valid coords
within_coords(Board, X-Y) :-
  length(Board, Size),
  NewSize is Size - 1,
  between(0, NewSize, X),
  between(0, NewSize, Y).

% check_color(+Board, +CoordsCurrent, +CoordsNext)
% check if the color from the next neigbor is the same as the current
check_color(Board, X1-Y1, Player) :-
  get_piece(Board,X1-Y1,Player-_).

% value(+GameState, +Player, -Value)
% scores the current game state
value([Board, _ , _ , _ , _ , LongestSequenceWhite-_], white, Value) :-
  findall(SeqScore, (
    within_coords(Board,X-Y),
    nth0(X, Board, Row),
    nth0(Y, Row, white-_),
    sequence_score(Board, X-Y, white, SeqScore)
    ), Scores),
  max_member(MaxScore, Scores),
  Value = LongestSequenceWhite + MaxScore.

value([Board, _ , _ , _ , _ , _-LongestSequenceBlack], black, Value) :-
  findall(SeqScore, (
    nth0(X, Board, Row),
    nth0(Y, Row, black-_),
    sequence_score(Board, X-Y, black, SeqScore)
    ), Scores),
  max_member(MaxScore, Scores),
  Value = LongestSequenceBlack + MaxScore.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sequence_score(Board, X-_, white, Score) :-
  length(Board, Size),
  HalfSize is Size // 2,
  X < HalfSize,
  Score is -X.
sequence_score(Board, X-_, white, Score) :-
  length(Board, Size),
  HalfSize is Size // 2,
  X >= HalfSize,
  Score is -(Size - X).
sequence_score(Board, _-Y, black, Score) :-
  length(Board, Size),
  HalfSize is Size // 2,
  Y < HalfSize,
  Score is -Y.
sequence_score(Board, _-Y, black, Score) :-
  length(Board, Size),
  HalfSize is Size // 2,
  Y >= HalfSize,
  Score is -(Size - Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% replace_long_sequence(+Board, +Coords, +LongestWhiteSequence, +Player , -NewLongestWhiteSequence)
% updates longest sequence if newly inserted piece increases it
replace_long_sequence(Board, X-Y, CurrentLongestSequence, Player, NewLongestSequence) :- 
  dfs_sequence(Board, [X-Y], Player, [], Length),
  max(CurrentLongestSequence, Length, NewLongestSequence).

% dfs_sequence(+Board, +ToVisit, +Player, +Visited, -Length)
dfs_sequence(_, [], _, _, 0).
dfs_sequence(Board, [X-Y | ToVisit], Player, Visited, Length) :-
  \+ member(X-Y, Visited),
  findall(Neighbor, (
      neighbor(X-Y, Neighbor),
      within_coords(Board, Neighbor),
      check_color(Board, Neighbor, Player),
      \+ member(Neighbor, ToVisit),
      \+ member(Neighbor, Visited)
    ), Neighbors),
  append(Neighbors, ToVisit, NewToVisit),
  dfs_sequence(Board, NewToVisit, Player, [X-Y | Visited], SubLength),
  Length is SubLength + 1.
dfs_sequence(Board, [_ | ToVisit], Player, Visited, Length) :-
  dfs_sequence(Board, ToVisit, Player, Visited, Length).

% correct_top_left_corner(+Direction, +Color, +Coords, -CorrectCoords)
% corrects the square where the dfs will perform
correct_top_left_corner(left, white, X-Y, X1-Y) :- X1 is X+1.
correct_top_left_corner(left, black,  X-Y , X-Y).

correct_top_left_corner(up, white, X-Y, X-Y1) :- Y1 is Y+1.
correct_top_left_corner(up, black, X-Y , X-Y).

correct_top_left_corner(down, white,  X-Y , X-Y).
correct_top_left_corner(down, black, X-Y, X-Y1) :- Y1 is Y+1.

correct_top_left_corner(right, white, X-Y , X-Y).
correct_top_left_corner(right, black, X-Y, X1-Y) :- X1 is X+1.

% choose_move(+GameState, +Level, -Move)
% returns the move chosen by the computer player
% for human players, it interacts with the user to read the move
choose_move([Board, white, hardBot-_, _-_, MaxLayer, LongestSequenceWhite-_], Move, Play) :-
  Play > 2,
  
  valid_moves([Board, _, _, _, MaxLayer, _], Moves),

  maplist(evaluate_move(Board, LongestSequenceWhite, white), Moves, ScoredMoves),
  
  max_member(_-X-Y-Orientation, ScoredMoves),
  Move = X-Y-Orientation.

choose_move([Board, black, _-_, hardBot-_, MaxLayer, _-LongestSequenceBlack], Move, Play) :-
  Play > 2,
  
  valid_moves([Board, _, _, _, MaxLayer, _], Moves),

  maplist(evaluate_move(Board, LongestSequenceBlack, black), Moves, ScoredMoves),
    
  max_member(_-X-Y-Orientation, ScoredMoves),
  Move = X-Y-Orientation.

choose_move([Board, black, _-_, hardBot-_, MaxLayer, _], X-Y-Orientation, 2) :-
  valid_moves([Board,_,_,_,MaxLayer,_],Moves),
  random_member(X-Y-Orientation, Moves).

choose_move([Board, white, hardBot-_, _-_, MaxLayer, _], X-Y-Orientation, 1) :-
  valid_moves([Board,_,_,_,MaxLayer,_],Moves),
  random_member(X-Y-Orientation, Moves).

choose_move([Board,white,easyBot-_,_-_,MaxLayer,_] , X-Y-Orientation, _) :- 
  valid_moves([Board,_,_,_,MaxLayer,_],Moves),
  random_member(X-Y-Orientation, Moves).
choose_move([Board,white,player-_,_-_,MaxLayer,_] , X-Y-Orientation, _) :-
  nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('White to move'), nl,
  write('--------------------'),nl,

  get_move([Board,_,_,_,MaxLayer,_],X-Y,Orientation).

choose_move([Board,black,_-_,easyBot-_,MaxLayer,_] , X-Y-Orientation, _) :- 
  valid_moves([Board,_,_,_,MaxLayer,_],Moves),
  random_member(X-Y-Orientation, Moves).
choose_move([Board,black,_-_,player-_,MaxLayer,_] , X-Y-Orientation, _) :- 
  nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('Black to move'), nl,
  write('--------------------'),nl,

  get_move([Board,_,_,_,MaxLayer,_],X-Y,Orientation).

% evaluate_move(+Board, +CurrentLongestSequence, +Player, +Piece, -ScoredPiece)
% evaluates a move and gives it a score
evaluate_move(Board, LongestSequenceWhite, white, X-Y-Orientation, Value-X-Y-Orientation) :-
  place_piece(Board, X-Y, Orientation, NewBoard),
  correct_top_left_corner(Orientation, white, X-Y, NewX-NewY),
  replace_long_sequence(NewBoard, NewX-NewY, LongestSequenceWhite, white, NewLongestSequenceWhite),
  NewGameState = [NewBoard, _, _-_, _-_, _, NewLongestSequenceWhite-_],
  value(NewGameState, white, Value).

evaluate_move(Board, LongestSequenceBlack, black, X-Y-Orientation, Value-X-Y-Orientation) :-
  place_piece(Board, X-Y, Orientation, NewBoard),
  correct_top_left_corner(Orientation, black, X-Y, NewX-NewY),
  replace_long_sequence(NewBoard, NewX-NewY, LongestSequenceBlack, black, NewLongestSequenceBlack),
  NewGameState = [NewBoard, _, _-_, _-_, _, _-NewLongestSequenceBlack],
  value(NewGameState, black, Value).
