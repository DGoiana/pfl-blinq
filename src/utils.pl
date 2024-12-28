% between(+Min,+Max,?Value)
% determines if a number is between range
between(Min,Max,Value) :-
  Value @>= Min,
  Value @=< Max.

% get_input(+Min,+Max,-Value)
% gets an input value from user until the value is in range (Min,Max)
get_input(Min,Max,Value) :-
  repeat,
  read(Value),
  number(Value),
  between(Min,Max,Value), !.