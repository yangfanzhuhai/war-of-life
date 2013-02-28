test_strategy_scores(0, _, _, 0, 0, 0, 0, 0, 0).

max(X, Y, X) :-
  X>=Y.

max(X, Y, Y) :-
  X<Y.

min(X, Y, X) :-
  X=<Y.

min(X, Y, Y) :-
  X>Y.

test_strategy_scores(N, St1, St2, Num_draws, Num_wins_b, Num_wins_r, Longest_move, Shortest_move, Total_moves, Total_time) :- 
  play(quiet, st1, st2, NumMoves, WinningPlayer),
  WinningPlayer == 'r',
  test_strategy_scores(N-1, St1, St2, Num_draws, Num_wins_b, Num_wins_r-1, Next_longest_move, Next_shortest_move, Total_moves - NumMoves, Total_time),
  max(NumMoves, Next_longest_move, Longest_move),
  min(NumMoves, Next_shortest_move, Shortest_move).
  

test_strategy_scores(N, St1, St2, Num_draws, Num_wins_b, Num_wins_r, Longest_move, Shortest_move, Total_moves, Total_time) :-
  play(quiet, st1, st2, NumMoves, WinningPlayer),
  WinningPlayer == 'b',
  test_strategy_scores(N-1, St1, St2, Num_draws, Num_wins_b-1, Num_wins_r, Next_longest_move, Next_shortest_move, Total_moves - NumMoves, Total_time),
  max(NumMoves, Next_longest_move, Longest_move),
  min(NumMoves, Next_shortest_move, Shortest_move).
  
test_strategy_scores(N, St1, St2, Num_draws, Num_wins_b, Num_wins_r, Longest_move, Shortest_move, Total_moves, Total_time) :-
  play(quiet, st1, st2, NumMoves, WinningPlayer),
  \+ WinningPlayer == 'b',
  \+ WinningPlayer == 'r',
  test_strategy_scores(N-1, St1, St2, Num_draws-1, Num_wins_b, Num_wins_r, Next_longest_move, Next_shortest_move, Total_moves - NumMoves, Total_time),
  (WinningPlayer == 'exhaustive' 
   ->
      Next_longest_move is Longest_move
   ;
      max(NumMoves, Next_longest_move, Longest_move)
   ),
  min(NumMoves, Next_shortest_move, Shortest_move).

test_strategy(N, St1, St2) :-
  test_strategy_scores(N, St1, St2, Num_draws, Num_wins_b, Num_wins_r, Longest_move, Shortest_move, Total_moves, Total_time),
  Avg_move is Total_moves/N,
  Avg_time is Total_time/N,
  write('Num_draws: '), write(Num_draws), write('\n'),
  write('Num_wins_blue: '), write(Num_wins_b), write('\n'),
  write('Num_wins_red: '), write(Num_wins_r), write('\n'),
  write('Longest_move: '), write(Longest_move), write('\n'),
  write('Shortest_move: '), write(Shortest_move), write('\n'),
  write('Avg_move: '), write(Avg_move), write('\n'),
  write('Avg_time: '), write(Avg_time), write('\n').
