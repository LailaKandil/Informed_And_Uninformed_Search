% Define the moves
move([X, Y], [NX, Y]) :- NX is X + 1. % Move down
move([X, Y], [NX, Y]) :- NX is X - 1. % Move up
move([X, Y], [X, NY]) :- NY is Y + 1. % Move right
move([X, Y], [X, NY]) :- NY is Y - 1. % Move left

% Define the heuristic
calculateH([X, Y], [GX, GY], H) :-
    DX is GX - X,
    DY is GY - Y,
    H is abs(DX) + abs(DY).

% Define the memberButBetter function
memberButBetter(Next, List, NewF):-
    findall(F, (member([Next,_,_,_,F], List)), Numbers),
    min_list(Numbers, MinOldF),
    MinOldF > NewF.

% Define allowed colors
allowedColor(r).
allowedColor(y).
allowedColor(b).

% Initialize row with color validation
initializeRow([], _, _).
initializeRow([Color|Cols], RowNum, ColNum) :-
    repeat,
    write('Enter color for cell ['), write(RowNum), write(','), write(ColNum), write('] (r, y, b): '), nl,
    read(Input),
    ( allowedColor(Input) -> Color = Input, ! ; write('Invalid color! Please enter r, y, or b.'), nl, fail ),
    NewColNum is ColNum + 1,
    initializeRow(Cols, RowNum, NewColNum).

initializeBoard([], _, _).
initializeBoard([Row|Rows], RowNum, Cols) :-
    RowNum >= 0,
    length(Row, Cols),
    initializeRow(Row, RowNum, 0), % Start column index from 0
    NewRowNum is RowNum + 1, % Increase row index by 1
    initializeBoard(Rows, NewRowNum, Cols). % Recursively initialize the next row

main :-
    write('Enter the number of rows: '), nl,
    read(Rows),
    write('Enter the number of columns: '), nl,
    read(Cols),
    RowNum is 0, % Start row index from 0
    length(Board, Rows),
    initializeBoard(Board, RowNum, Cols),
    write('Enter the start position (e.g., [0,0]): '), nl,
    read(Start),
    write('Enter the goal position (e.g., [2,2]): '), nl,
    read(Goal),
    search([[Start, null, 0, 0, 0]], [], Goal, Board).

% Define the search function
search(Open, Closed, Goal, Board):-
    getBestState(Open, [CurrentState,Parent,G,H,F], _),
    CurrentState = Goal,
    write("Search is complete!"), nl,
    printSolution([CurrentState,Parent,G,H,F], Closed), !.
search([], _, _, _):-
    write("No path exists."), nl, !.
search(Open, Closed, Goal, Board):-
    getBestState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(Board, CurrentNode, TmpOpen, Closed, Goal, Children),
    addChildren(Children, TmpOpen, NewOpen),
    append(Closed, [CurrentNode], NewClosed),
    search(NewOpen, NewClosed, Goal, Board).

% Define the getAllValidChildren function
getAllValidChildren(Board, Node, Open, Closed, Goal, Children):-
    findall(Next, getNextState(Board, Node, Open, Closed, Goal, Next), Children).

getNextState(Board, [State,_,G,_,_],Open,Closed,Goal,[Next,State,NewG,NewH,NewF]):-
    move(State, Next),
    valid_move(Board, State, Next),
    calculateH(Next, Goal, NewH),
    NewG is G + 1,
    NewF is NewG + NewH,
    ( not(member([Next,_,_,_,_], Open)) ; memberButBetter(Next,Open,NewF) ),
    ( not(member([Next,_,_,_,_],Closed));memberButBetter(Next,Closed,NewF)).
    
% Define the addChildren and getBestState functions
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

% Define the findMin function
findMin([X], X):- !.
findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_, _, _, _, HeadF],
    TmpMin = [_, _, _, _, TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).

% Define the printSolution function
printSolution([State, null, G, H, F],_):-
    write([State, G, H, F]), nl.
    
printSolution([State, Parent, G, H, F], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write([State, G, H, F]), nl.

% Define the valid_move predicate
valid_move(Board, [X, Y], [NX, NY]) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Color),
    length(Row, RowLength),
    length(Board, BoardLength),
    NX >= 0, NX < BoardLength, NY >= 0, NY < RowLength,
    nth0(NX, Board, NextRow),
    nth0(NY, NextRow, NextColor),
    Color = NextColor.
