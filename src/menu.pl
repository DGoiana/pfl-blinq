%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MENU
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% level_convert(+LevelInput,-BotLevel)
% converts input into bot level
level_convert(1,easyBot).
level_convert(2,hardBot).

% get_level(-Level)
% gets bot level
get_level(Level) :-
  write('1- Easy'), nl,
  write('2- Hard'), nl,
  write('Bot Level:'),
  get_input(1,2,BotLevel),
  level_convert(BotLevel,Level).

% get_type(+TypeInput,-GameType)
% gets the game type
get_type(4,player-player).
get_type(3,GameType) :-
  get_level(BotLevel),
  GameType = player-BotLevel.
get_type(2,GameType) :-
  get_level(BotLevel),
  GameType = BotLevel-player.
get_type(1,GameType) :-
  get_level(BotLevel1),
  get_level(BotLevel2),
  GameType = BotLevel1-BotLevel2.

convert_orientation(1,left).
convert_orientation(2,right).
convert_orientation(3,up).
convert_orientation(4,down).

% get_orientation(-Orientation)
% gets the desired orientation of a move from the user
get_orientation(Orientation) :-
  write('Possible Orientations:'),nl,
  nl,
  write('1- bw  2- wb'), nl,
  write('   bw     wb'), nl,
  nl,
  write('3- bb  4- ww'), nl,
  write('   ww     bb'), nl,
  nl,
  write('Orientation:'),
  get_input(1,4,OrientationInput),
  convert_orientation(OrientationInput,Orientation).

% get_orientation(+GameState,-Coords,-Orientation)
% gets the desired move from the user
get_move([Board,_,_,_,MaxLayer,_],X-Y,Orientation) :-
  valid_moves([Board,_,_,_,MaxLayer,_],ValidMoves),

  length(Board,BoardSize),
  repeat,
  write('X Coord:'),
  get_input(0,BoardSize,XInput),
  write('Y Coord:'),
  get_input(0,BoardSize,YInput),
  get_orientation(Orientation),
  convert_coords(XInput-YInput,BoardSize,X-Y),
  validate_valid_move(X-Y-Orientation,ValidMoves),
  !.

validate_valid_move(X-Y-Orientation,ValidMoves) :-
  check_valid_move(X-Y-Orientation,ValidMoves).
  
validate_valid_move(X-Y-Orientation,ValidMoves) :-
  \+ check_valid_move(X-Y-Orientation,ValidMoves),
  write('Invalid Move. Try again'), nl,
  fail.


% menu(-GameConfig)
% prints menu and defines GameConfig
menu([BoardSize,GameType]) :- 
  write('Blinq'), nl,
  write('Board Size (3-10): '),
  get_input(3,10,BoardSize),
  write('Game Type'), nl,
  write('(White-Black)'),nl,
  write('1- Bot-Bot'), nl,
  write('2- Bot-Player'), nl,
  write('3- Player-Bot'), nl,
  write('4- Player-Player'), nl,
  write('5- Exit'), nl,
  write('Type:'),
  get_input(1,5,Type),
  exit(Type),
  get_type(Type,GameType).

% exit(Type)
% exists the game
exit(5) :- !,fail.
exit(_).
