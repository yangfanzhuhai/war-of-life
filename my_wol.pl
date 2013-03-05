/*
 * Prolog AI Coursework:
 */
:- use_module(library(system)).

/* Just for flexibility purposes: */
game_type(quiet).


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

/* Delete Num of Elem from a List: */
delete_some(List, _, 0, List).

delete_some([Elem|Rest], Elem, Num, Result) :-
  Num>0,
  Num2 is (Num-1),
  delete_some(Rest, Elem, Num2, Result).
  
delete_some([Head|Rest], Elem, Num, [Head|Result]) :-
  \+ Head==Elem,
  Num>0,
  delete_some(Rest, Elem, Num, Result).

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
  write('Comparing: '),
  write(St1),
  write('(Payer 1) with '),
  write(St2),
  write(' (Player 2)\n'),
  now(StartingTime),
  test_strategy_scores(N, St1, St2, MovesList, WinList),
  now(EndingTime),
  list_elem_count(WinList, 'draw', Num_normal_draws),
  list_elem_count(WinList, 'exhaust', Num_exhaust_draws),
  list_elem_count(WinList, 'stalemate', Num_stalemate_draws),
  list_elem_count(WinList, 'b', Num_wins_b),
  list_elem_count(WinList, 'r', Num_wins_r),
  min(MovesList, Shortest_game),
  sum_list(MovesList, SumMoves),
  list_length(MovesList, LenMovesList),
  Total is (Num_normal_draws + Num_exhaust_draws + 
            Num_stalemate_draws + Num_wins_b + Num_wins_r),
  Num_draws is (Num_normal_draws + Num_exhaust_draws + 
                Num_stalemate_draws),
  delete_some(MovesList, 250, Num_exhaust_draws, 
              Non_exhaustive_MovesList),
  max(Non_exhaustive_MovesList, Longest_game),            
  Avg_move is SumMoves / LenMovesList,
  Avg_time is (EndingTime-StartingTime)*1000/N,
  write('List Length: '), write(LenMovesList), write('\n'),
  write('Total: '), write(Total), write('\n'),
  write('Num_draws: '), write(Num_draws), write('\n'),
  write('Num_normal_draws: '), write(Num_normal_draws), write('\n'),
  write('Num_exhaust_draws: '), write(Num_exhaust_draws), write('\n'),  
  write('Num_stalemate_draws: '), write(Num_stalemate_draws), write('\n'),
  write('Num_wins_blue: '), write(Num_wins_b), write('\n'),
  write('Num_wins_red: '), write(Num_wins_r), write('\n'),
  write('Longest_game: '), write(Longest_game), write('\n'),
  write('Shortest_game: '), write(Shortest_game), write('\n'),
  write('Avg_move: '), write(Avg_move), write('\n'),
  write('Avg_time: '), write(Avg_time), write('\n').
  
  
/*
 * AI Strategies: 
 */


bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  implement_strategy(PlayerColour, CurrentBoardState, NewBoardState, Move, bloodlust).

self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  implement_strategy(PlayerColour, CurrentBoardState, NewBoardState, Move, self_preservation).

land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  implement_strategy(PlayerColour, CurrentBoardState, NewBoardState, Move, land_grab).
  
minimax(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  implement_strategy(PlayerColour, CurrentBoardState, NewBoardState, Move, minimax).

get_best_move([], _, _, _, _, Move, NewBoardState, Score).  
  
/* When the Curr_Move is the best move so far. */
get_best_move([Curr_Move | List_Moves],PlayerColour, My_Alive, Op_Alive, Strategy, Curr_Move, NewBoardState, HighestScore) :-
  alter_board(Curr_Move, My_Alive, New_My_Alive),
  compose_board(PlayerColour, NewMyAlives, Op_Alive, NewBoardState),
  next_generation(NewBoardState, NewGenerationBoard),
  get_score(Strategy, NewGenerationBoard, HighestScore), /*TODO this */
  get_best_move(List_Moves, PlayerColour, My_Alive, Op_Alive, Strategy, Move, NewState, Score),
  HighestScore>=Score.

/* When the Curr_Move is not the best move so far. */
get_best_move([Curr_Move | List_Moves],PlayerColour, My_Alive, Op_Alive, Strategy, Move, NewBoardState, HighestScore) :-
  alter_board(Curr_Move, My_Alive, New_My_Alive),
  compose_board(PlayerColour, NewMyAlives, Op_Alive, NewBoardState),
  next_generation(NewState, NewGenerationBoard),
  get_score(Strategy, NewGenerationBoard, Score), /*TODO this */
  get_best_move(List_Moves, PlayerColour, My_Alive, Op_Alive, Strategy, Move, NewBoardState, HighestScore),
  HighestScore>Score. 
  
implement_strategy(PlayerColour, CurrentBoardState, NewBoardState, Move, Strategy) :-
  separate_board(PlayerColour, CurrentBoardState, My_Alive, Op_Alive),  
  possible_moves(My_Alive, Op_Alive, PosMoves),
  get_best_move(PossMoves, PlayerColour, My_Alive, Op_Alive, Strategy, Move, NewBoardState, HighestScore).
/*  alter_board(Move, My_Alive, NewMyAlives),
  compose_board(PlaycerColour, NewMyAlives, Op_Alive, NewBoardState).*/
  
  
  
  
  
  
  
  
