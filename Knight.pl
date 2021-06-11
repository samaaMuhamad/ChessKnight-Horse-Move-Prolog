%Moves
move([I, J], [M, N2],List) :-
    I + 1 < 8, 
    J + 2 < 8, 
    M is I + 1,
    \+ member([M,J],List),
    N is J + 1,
    \+ member([M,N],List),
    N2 is N + 1,
    \+ member([M,N2],List).
    
move([I, J], [M2, N],List) :-
    I + 2 < 8,
    J + 1 < 8,
    M is I + 1,
    \+ member([M,J],List),
    M2 is M + 1,
    \+ member([M2,J],List),
    N is J + 1,
    \+ member([M2,N],List). 
    
move([I, J], [M2, N],List) :-
    I + 2 < 8,
    J - 1 >= 0,
    M is I + 1,
    \+ member([M,J],List),
    M2 is M + 1,
    \+ member([M2,J],List),
    N is J - 1,
    \+ member([M2,N],List). 
    
move([I, J], [M, N2],List) :-
    I + 1 < 8,
    J - 2 >= 0,
    M is I + 1,
    \+ member([M,J],List),
    N is J - 1,
    \+ member([M,N],List),
    N2 is N - 1,
    \+ member([M,N2],List). 
    
move([I, J], [M, N2],List) :-
    I - 1 >= 0,
    J - 2 >= 0,
    M is I - 1,
    \+ member([M,J],List),
    N is J - 1,
    \+ member([M,N],List),
    N2 is N - 1,
    \+ member([M,N2],List). 
    
move([I, J], [M2, N],List) :-
    I - 2 >= 0,
    J - 1 >= 0,
    M is I - 1,
    \+ member([M,J],List),
    M2 is M - 1,
    \+ member([M2,J],List),
    N is J - 1,
    \+ member([M2,N],List). 
    
move([I, J], [M2, N],List) :-
    I - 2 >= 0,
    J + 1 < 8,
    M is I - 1,
    \+ member([M,J],List),
    M2 is M - 1,
    \+ member([M2,J],List),
    N is J + 1,
    \+ member([M2,N],List). 
    
move([I, J], [M, N2],List) :-
    I - 1 >= 0,
    J + 2 < 8,
    M is I - 1,
    \+ member([M,J],List),
    N is J + 1,
    \+ member([M,N],List),
    N2 is N + 1,
    \+ member([M,N2],List). 

/*
grid(8,8)
play(start,Goal,BlockList)
*/


play(Start,Goal,List):-
      getHeuristic(Start, H, Goal),
      path([[Start,null, 0, H, H]],[],Goal,List).%open, closed, goal, path_cost, heuristic, total cost


%main predicate that takes open list, closed list and goal state

path([], _, _,_):-
      write('No solution'),nl,!.

path(Open, Closed, Goal,List):-
      getBestChild(Open, [Goal, Parent, PC, H, TC], RestOfOpen),
      write('A solution is found'),  nl ,
      printsolution([Goal,Parent, PC, H, TC], Closed),!.

path(Open, Closed, Goal,List):-

      getBestChild(Open, [State, Parent, PC, H, TC], RestOfOpen),

      getchildren(State, Open, Closed, Children, PC, Goal,List),

      addListToOpen(Children , RestOfOpen, NewOpen),

      path(NewOpen, [[State, Parent, PC, H, TC] | Closed], Goal,List), !.



%gets Children of State that aren't in Open or Close

getchildren(State, Open ,Closed , Children, PC, Goal, List):-

      bagof(X, moves( State, Open, Closed, X, PC, Goal,List), Children) .

getchildren(_,_,_, [],_,_,_).



%adds children to open list (without best child) to form new open list

addListToOpen(Children, [], Children).

addListToOpen(Children, [H|Open], [H|NewOpen]):-

      addListToOpen(Children, Open, NewOpen).



%gets the best state of the open list and another list without this best state

%first parameter is the open list

%second parameter is the best child

%third parameter is the open list without the best child

getBestChild([Child], Child, []):- !.

getBestChild(Open, Best, RestOpen):-

  getBestChild1(Open, Best),

  removeFromList(Best, Open, RestOpen).



%gets the best state of the open list

getBestChild1([State], State):- !.

getBestChild1([State|Rest], Best):-

  getBestChild1(Rest, Temp),

  getBest(State, Temp, Best).



%compares two states with each other (according to their Total cost) and returns the state with lower total cost TC

getBest([State, Parent, PC, H, TC], [_, _, _, _, TC1], [State, Parent, PC, H, TC]):-

  TC < TC1, !.

getBest([_, _, _, _, _], [State1, Parent1, PC1, H1, TC1], [State1, Parent1, PC1, H1, TC1]).



%removes an element (usually the best state) from a list (open list) and returns a new list

removeFromList(_, [], []).

removeFromList(H, [H|T], V):-

  !, removeFromList(H, T, V).

removeFromList(H, [H1|T], [H1|T1]):-

  removeFromList(H, T, T1).



%gets next state given the current state

moves( State, Open, Closed,[[R,C],State, NPC, H, TC], PC, Goal, List):-

      move(State,[R,C],List),

      \+ member([[R,C], _, _, _, _],Open),

      \+ member([[R,C], _, _, _, _],Closed),

      NPC is PC + 1,

      getHeuristic([R,C], H, Goal),

      TC is NPC + H.



%calculate heuristic of some state

getHeuristic( [], 0, []):-!.

getHeuristic([H1|T1],H,[H2|T2]):-
  X is abs(H1 - H2),
  Y is abs(T1 - T2), 
  H is (X + Y ).


%prints the path from start state to goal state

printsolution([State, null, PC, H, TC],_):-

      write(State), write(' PC: '), write(PC), write(' H:'), write(H), write(' TC: '), write(TC), nl.

printsolution([State, Parent, PC, H, TC], Closed):-

      member([Parent, GrandParent, PC1, H1, TC1], Closed),

      printsolution([Parent, GrandParent, PC1, H1, TC1], Closed),

  write(Parent),	write(State), write(' !!PC: '), write(PC), write(' H:'), write(H), write(' TC: '), write(TC), nl.



