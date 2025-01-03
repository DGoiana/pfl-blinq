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
  game_loop(GameState).

% game_loop(+GameState)
% loop of the main game
game_loop([Board,_,_-0,_-_,_,_]) :-
  show_winner(draw),
  display_board(Board,[]), !.
game_loop([Board,_,_-_,_-0,_,_]) :-
  show_winner(draw),
  display_board(Board,[]), !.
game_loop(GameState) :-
  GameState = [Board,CurrentPlayer,_,_,_,_],
  game_over(GameState,CurrentPlayer),
  show_winner(CurrentPlayer),
  display_board(Board,[]), !.
game_loop(GameState) :-
  write(GameState), nl,
  display_game(GameState),
  write('bip'),
  choose_move(GameState, _, Move),
  write('bop'),
  move(GameState,Move,NewGameState),
  game_loop(NewGameState).

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
% case 1: one of the players win (kinda dfs)
% case 2: one player loses all pieces (if the other player )
% case 3: both players lose all pieces (draw)
game_over([Board,_,_,_,_,_],draw) :-
  \+ check_winner(Board,black),
  \+ check_winner(Board,white).
game_over([Board,_,_,_,_,_],white) :-
  check_winner(Board,white).
game_over([Board,_,_,_,_,_],black) :-
  check_winner(Board,black).

% check_winner(+Board, -Winner)
% checks if anyone has won the game in the current board
check_winner(Board, Player) :- 
  dfs(Board, Player, Winner, _),
  Player = Winner.

% dfs(+Board, +Player, -Won)
% performs a dfs to see if the player has already won the game
dfs(Board, white, Won, Length) :-
  findall(0-Y, (nth0(0, Board, Row), (nth0(Y, Row, white-_))), Pieces),
  member(Start, Pieces),
  dfs_visit(Board, white, [Start], [], Won, Length).

dfs(Board, black, Won, Length) :-
  findall(X-0, (nth0(X, Board, Row), (nth0(0, Row, black-_))), Pieces),
  member(Start, Pieces),
  dfs_visit(Board, black, [Start], [], Won, Length).
  
% dfs_visit(+Board, +Player, +ToVisit, +AlreadyVisited, -Won)
% helper function for dfs traversal
dfs_visit(_, _, [], Visited, false, Length) :-
  length(Visited, Length).
dfs_visit(Board, Player, [Current|_], _, Player, Length) :-
  Current = X-Y,
  end_game(Board, X-Y, Player),
  Length is 1.
dfs_visit(Board, Player, [Current|ToVisit], Visited, Won, Length) :-
  Current = X-Y,
  findall(Next, (neighbor(X-Y, Next), within_coords(Board, Next), \+ member(Next, Visited), \+ member(Next, ToVisit), check_color(Board, Current, Next)), Neighbors),
  append(Neighbors, ToVisit, NewToVisit),
  sort(NewToVisit, SortedToVisit),
  dfs_visit(Board, Player, SortedToVisit, [Current|Visited], Won, Length).

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
% scores the current game state

value([Board, _ , _ , _ , _ , LongestSequenceWhite-_], white, Value) :-
  findall(SeqScore, (
    within_coords(Board,X-Y),
    nth0(X, Board, Row),
    nth0(Y, Row, white-_),
    sequence_score(Board, X-Y, white, SeqScore)
    ), Scores),
  max_member(MaxScore, Scores),
  Value is MaxScore + LongestSequenceWhite * 0.1.

value([Board, _ , _ , _ , _ , _-LongestSequenceBlack], black, Value) :-
  findall(SeqScore, (
    nth0(X, Board, Row),
    nth0(Y, Row, black-_),
    sequence_score(Board, X-Y, black, SeqScore)
    ), Scores),
  max_member(MaxScore, Scores),
  Value is MaxScore + LongestSequenceBlack * 0.1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sequence_score(Board, X-Y, white, Score) :-
  length(Board, Size),
  HalfSize is Size // 2,
  X < HalfSize,
  Score is -X.
sequence_score(Board, X-Y, white, Score) :-
  length(Board, Size),
  HalfSize is Size // 2,
  X >= HalfSize,
  Score is -(Size - X).
sequence_score(Board, X-Y, black, Score) :-
  length(Board, Size),
  HalfSize is Size // 2,
  Y < HalfSize,
  Score is -Y.
sequence_score(Board, X-Y, black, Score) :-
  length(Board, Size),
  HalfSize is Size // 2,
  Y >= HalfSize,
  Score is -(Size - Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% replace_long_sequence(+Board, +Coords, +LongestWhiteSequence, +Player , -NewLongestWhiteSequence)
% updates longest sequence if newly inserted piece increases it
replace_long_sequence(Board, X-Y, CurrentLongestSequence, Player, NewLongestSequence) :- 
  dfs_visit(Board, Player, [X-Y], [], _, Length),
  max(CurrentLongestSequence, Length, NewLongestSequence).

% choose_move(+GameState, +Level, -Move)
% returns the move chosen by the computer player
% for human players, it interacts with the user to read the move

% [Board, _ , _ , _ , _ , _-LongestSequenceBlack]

choose_move([Board, white, hardBot-WhitePiecesLeft, _-_, MaxLayer,LongestSequenceWhite-_], hard, Move) :-
  valid_moves([Board, _, _, _, MaxLayer,_], Moves),
  findall(Value-PossibleMove, (
    member(PossibleMove, Moves),
    PossibleMove = X-Y-Orientation,
    place_piece(Board, X-Y, Orientation, NewBoard),
    replace_long_sequence(NewBoard, X-Y, LongestSequenceWhite, white, NewLongestSequenceWhite),
    NewGameState = [NewBoard, _ , _-_ , _-_ , _ , NewLongestSequenceWhite-_],
    value(NewGameState, white, Value)
  ), ScoredMoves),
  max_member(_-Move, ScoredMoves).

choose_move([Board, black, _-_, hardBot-BlackPiecesLeft, MaxLayer, _-LongestSequenceBlack], hard, Move) :-
  valid_moves([Board, _, _, _, MaxLayer,_], Moves),
  findall(Value-PossibleMove, (
    member(PossibleMove, Moves), 
    PossibleMove = X-Y-Orientation,
    place_piece(Board, X-Y, Orientation, NewBoard),
    replace_long_sequence(NewBoard, X-Y, LongestSequenceBlack, black, NewLongestSequenceBlack),
    NewGameState = [NewBoard, _ , _-_ , _-_ , _ , _-NewLongestSequenceBlack],
    value(NewGameState, black, Value)
  ), ScoredMoves),
  max_member(_-Move, ScoredMoves).

choose_move([Board,white,easyBot-_,_-_,MaxLayer,_], _ , X-Y-Orientation) :- 
  valid_moves([Board,_,_,_,MaxLayer,_],Moves),
  random_member(X-Y-Orientation, Moves).
choose_move([Board,white,player-_,_-_,MaxLayer,_], _ , X-Y-Orientation) :-
  nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('White to move'), nl,
  write('--------------------'),nl,

  get_move([Board,_,_,_,MaxLayer,_],X-Y,Orientation).
/* 
choose_move([Board,black,WhiteType-WhitePieces,hardBot-BlackPieces,MaxLayer],X-Y) :- .
*/
choose_move([Board,black,_-_,easyBot-_,MaxLayer,_], _ , X-Y-Orientation) :- 
  valid_moves([Board,_,_,_,MaxLayer,_],Moves),
  random_member(X-Y-Orientation, Moves).
choose_move([Board,black,_-_,player-_,MaxLayer,_], _ , X-Y-Orientation) :- 
  nl,
  write('Blinq'),nl,
  write('--------------------'),nl,
  write('Black to move'), nl,
  write('--------------------'),nl,

  get_move([Board,_,_,_,MaxLayer,_],X-Y,Orientation).
