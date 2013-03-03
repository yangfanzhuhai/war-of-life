/*
 * Prolog AI Coursework:
 */
:- use_module(library(system)).

/* Just for flexibility purposes: */
game_type(verbose).


/* Min and max functions for lists: */
max([Max], Max).
max([X1,X2|T], X) :-
  X1>=X2,
  max([X1|T], X).

max([X1,X2|T], X) :-
  X1<X2,
  max([X2|T], X).


min([Min], Min).
min([X1,X2|T], X) :-
  X1=<X2,
  min([X1|T], X).

min([X1,X2|T], X) :-
  X1>X2,
  min([X2|T], X).


/* Counts the number of elements in a list: */
list_elem_count([], _, 0).

list_elem_count([El|T], El, N) :-
  list_elem_count(T, El, N2),
  N is N2+1.

list_elem_count([X|T], El, N) :-
  \+ X == El,
  list_elem_count(T, El, N).

/* Summing the elements of a list: */
sum_list([], 0).
sum_list([X|T], Sum) :-
  sum_list(T, Sum2),
  Sum is Sum2+X.

/* Length of a list: */
list_length([], 0).
list_length([_|T], Len) :-
  list_length(T, Len2),
  Len is Len2+1.

/* Base Case: */
test_strategy_scores(0, _, _, [], []).

/* Recursion: */
test_strategy_scores(N, St1, St2, [CurrMoves|Moves], [CurrWinner|Wins]) :-
  N>0,
  game_type(Type),
  play(Type, St1, St2, CurrMoves, CurrWinner),
  N2 is N-1,
  test_strategy_scores(N2, St1, St2, Moves, Wins).


test_strategy(N, St1, St2) :-
  write('Comparing: '), write(St1), write('(Payer 1) with '), write(St2), write(' (Player 2)\n'),
  now(StartingTime),
  test_strategy_scores(N, St1, St2, MovesList, WinList),
  now(EndingTime),
  list_elem_count(WinList, 'draw', Num_draws),
  list_elem_count(WinList, 'b', Num_wins_b),
  list_elem_count(WinList, 'r', Num_wins_r),
  max(MovesList, Longest_game),
  min(MovesList, Shortest_game),
  sum_list(MovesList, SumMoves),
  list_length(MovesList, LenMovesList),
  Avg_move is SumMoves / LenMovesList,
  Avg_time is (EndingTime-StartingTime)*1000/N,
  write('Num_draws: '), write(Num_draws), write('\n'),
  write('Num_wins_blue: '), write(Num_wins_b), write('\n'),
  write('Num_wins_red: '), write(Num_wins_r), write('\n'),
  write('Longest_game: '), write(Longest_game), write('\n'),
  write('Shortest_game: '), write(Shortest_game), write('\n'),
  write('Avg_move: '), write(Avg_move), write('\n'),
  write('Avg_time: '), write(Avg_time), write('\n').
