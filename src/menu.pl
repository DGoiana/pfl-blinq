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
get_type(3,player-player).
get_type(2,GameType) :-
  get_level(BotLevel),
  GameType = BotLevel-player.
get_type(1,GameType) :-
  get_level(BotLevel1),
  get_level(BotLevel2),
  GameType = BotLevel1-BotLevel2.

% menu(-GameConfig)
% prints menu and defines GameConfig
menu([GameSize,GameType]) :- 
  write('Blinq'), nl,
  write('Game Size:'),
  get_input(1,10,GameSize),
  write('Game Type'), nl,
  write('1- Bot-Bot'), nl,
  write('2- Bot-Player'), nl,
  write('3- Player-Player'), nl,
  write('Type:'),
  get_input(1,4,Type),
  get_type(Type,GameType).