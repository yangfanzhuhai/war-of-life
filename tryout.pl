delete_some(List, _, 0, List).

delete_some([Elem|Rest], Elem, Num, Result) :-
  Num>0,
  Num2 is (Num-1),
  delete_some(Rest, Elem, Num2, Result).
  
delete_some([Head|Rest], Elem, Num, [Head|Result]) :-
  \+ Head==Elem,
  Num>0,
  delete_some(Rest, Elem, Num, Result).
