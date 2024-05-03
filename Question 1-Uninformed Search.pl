%Request Grid Size from user
grid_size(N, M) :-
    write('Enter the number of rows (n): '),
    read(N),
    write('Enter the number of columns (m): '),
    read(M).

create_grid(N, M, Grid) :-
    create_grid(N, M, [], Grid).

create_grid(0, _, Grid, Grid).
create_grid(N, M, Acc, Grid) :-
    create_row(N,M, [], Row),
    append(Row,Acc, NewAcc),
    N1 is N - 1,
    create_grid(N1, M, NewAcc, Grid).

create_row(_,0, Row, Row).
create_row(CurrentRow,M, Acc, Row) :-
    CurrentRow > 0,
    M > 0,
    format('Enter a color for position (~w, ~w) (red/yellow/blue): ', [CurrentRow, M]),
    read(Color),
    (Color = red ; Color = yellow; Color = blue),
    append([(CurrentRow,M,Color)],Acc, NewAcc),
    M1 is M - 1,
    create_row(CurrentRow,M1, NewAcc, Row).


print_grid([]).
print_grid([Color|Colors]) :-
    write('('),
    write(Color),
    write(') '),
    print_grid(Colors).


% Finding the cycle
% Define valid moves within the grid bound
move((X,Y,CurrentNodeColor), (NX, Y, CurrentNodeColor)) :- NX is X + 1. % Move down
move((X,Y,CurrentNodeColor), (NX, Y, CurrentNodeColor)) :- NX is X - 1. % Move up
move((X,Y,CurrentNodeColor), (X,NY,CurrentNodeColor)) :- NY is Y - 1. % Move left
move((X,Y,CurrentNodeColor), (X, NY, CurrentNodeColor)) :- NY is Y + 1. % Move right


search(CurrentState, Grid, Visited, Path, Parent, Goal) :-
    move(CurrentState, NextState),
    not(member(NextState, Parent)),
    member(NextState, Grid),
    not(member(NextState, Visited)),
    append(Visited, [NextState], NewVisited),
    append(Path, [NextState], NewPath),
    search(NextState, Grid, NewVisited, NewPath, [CurrentState], Goal).

search(CurrentState, _, _, Path, Parent, Goal) :-
    move(CurrentState, Goal),
    not(member(Goal, Parent)),
    append(Path, [Goal], NewPath),
	write("Search is complete!"), nl,
    write(NewPath).


trySearchStartPoint(Grid,FirstNode) :-
    search(FirstNode, Grid, [FirstNode], [FirstNode], [], FirstNode).


%Main Predicate
main:-
    grid_size(N, M),
    create_grid(N, M, Grid),
    writeln('Printing grid'),
    print_grid(Grid),
    writeln(''),
    member(X,Grid),
    trySearchStartPoint(Grid,X),!.
