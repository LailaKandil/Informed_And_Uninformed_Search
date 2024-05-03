main :-
    write('Enter the number of rows: '), nl,
    read(Rows),
    write('Enter the number of columns: '), nl,
    read(Cols),
    % Initialize the row index to 0
    RowNum is 0,
    length(Board, Rows),  % Create a list of the specified number of rows
    initializeBoard(Board, RowNum, Cols),
    write('Enter the start cell (e.g., [0,0]): '), nl,
    read(Start),
    write('Enter the goal cell (e.g., [2,2]): '), nl,
    read(Goal),
    search([[Start, null, 0, 0, 0]], [], Goal, Board).

% Define allowed colors
allowedColor(r).
allowedColor(y).
allowedColor(b).

% Initialize row with color validation
initializeRow([], _, _).
initializeRow([Color|Cols], RowNum, ColNum) :-
    repeat, % Repeat until a valid color is entered
    write('Enter color for cell ['), write(RowNum), write(','), write(ColNum), write('] (r, y, b): '), nl,
    read(Input),
    % If the entered color is allowed, assign it to the current cell; otherwise, prompt the user to enter a valid color
    ( allowedColor(Input) -> Color = Input, ! ; write('Invalid color! Please enter r, y, or b.'), nl, fail ),
    NewColNum is ColNum + 1, % Move to the next column
    initializeRow(Cols, RowNum, NewColNum).

% Initialize the board with the specified number of rows and columns
initializeBoard([], _, _).
initializeBoard([Row|Rows], RowNum, Cols) :-
    RowNum >= 0,
    length(Row, Cols), % Create a list of the specified number of columns
    initializeRow(Row, RowNum, 0), % Start column index from 0
    NewRowNum is RowNum + 1, % Increase row index by 1
    initializeBoard(Rows, NewRowNum, Cols).

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

% Define the search function
search(Open, Closed, Goal, _):-
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
    Head = [_,_,_,_,HeadF],
    TmpMin = [_,_,_,_,TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).

% Define a predicate to check if a move is valid
valid_move(Board, [X, Y], [NX, NY]) :-
    % Get the color of the current cell
    nth0(X, Board, Row),
    nth0(Y, Row, Color),
    % Check if the next cell is within the board
    length(Row, RowLength),
    length(Board, BoardLength),
    NX >= 0, NX < BoardLength, NY >= 0, NY < RowLength,
    % Get the color of the next cell
    nth0(NX, Board, NextRow),
    nth0(NY, NextRow, NextColor),
    % Check if the color of the next cell is the same as the color of the current cell
    Color = NextColor.

% Define the printSolution function
printSolution([State, null, _, _, _],_):-
 write("The correct path is: "),
    write(State).

printSolution([State, Parent, _, _, _], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write(' - '),write(State).