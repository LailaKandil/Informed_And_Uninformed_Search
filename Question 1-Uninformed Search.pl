grid_size(N, M) :-
    write('Enter the number of rows (n): '),
    read(N),
    write('Enter the number of columns (m): '),
    read(M).

create_row(0,_,_, Row, Row).
create_row(M, N,Temp, Acc, Row) :-
    M > 0,
    format('Enter a color for position (~w, ~w) (red/yellow/blue): ', [N, Temp]),
    read(Color),
    member(Color, [red, yellow, blue]),
    append(Acc, [Color], NewAcc),
    M1 is M - 1,
    Temp1 is Temp+1,
    create_row(M1, N,Temp1, NewAcc, Row).

create_grid(N, M, Grid) :-
    create_grid(N, M, 1, [], Grid).

create_grid(0, _, _, Grid, Grid).
create_grid(N, M, RowNum, Acc, Grid) :-
    create_row(M, RowNum,1, [], Row),
    append(Acc, [Row], NewAcc),
    N1 is N - 1,
    RowNum1 is RowNum + 1,
    create_grid(N1, M, RowNum1, NewAcc, Grid).

print_grid([]).
print_grid([Row|Rows]) :-
    print_row(Row, 1),
    nl,
    print_grid(Rows).

print_row([], _).
print_row([Color|Colors], Position) :-
    write('( ')
    write(Color),
    write(') '),
    NextPosition is Position + 1,
    print_row(Colors, NextPosition).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define valid moves within the grid bounds
% Define valid moves within the grid bounds
move((X1, Y1, Color), (X2, Y2, Color), N, M) :-
    X2 is X1 + 1, X2 < N, Y2 is Y1, Y2 < M.
move((X1, Y1, Color), (X2, Y2, Color), _, _) :-
    X2 is X1 - 1, X2 >= 0, Y2 is Y1.
move((X1, Y1, Color), (X2, Y2, Color), _, M) :-
    X2 is X1, X2 < N, Y2 is Y1 + 1, Y2 < M.
move((X1, Y1, Color), (X2, Y2, Color), N, _) :-
    X2 is X1, X2 < N, Y2 is Y1 - 1, Y2 >= 0.

% Depth-first search to find color cycle
search(_, [], _, _, _) :- fail. % No cycle found
search(CurrentState, Visited, N, M, Goal):-
    member(CurrentState, Visited),
    write("Cycle found!"), nl,
    write(Visited), nl, write(CurrentState).
search(CurrentState, Visited, N, M, Goal):-
    move(CurrentState, NextState, N, M),
    not(member(NextState, Visited)),
    append(Visited, [CurrentState], NewVisited),
    search(NextState, NewVisited, N, M, Goal).

% Start the search for color cycle
find_color_cycle(InitialColor, N, M) :-
    search((0, 0, InitialColor), [], N, M, (N, M, InitialColor)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main :-
    grid_size(N, M),
    create_grid(N, M, Grid),
    print_grid(Grid),
    %find_color_cycle(red, N, M),
    writeln('Done.').
