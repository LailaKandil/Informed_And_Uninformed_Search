%Request Grid Size from user
grid_size(N, M) :-
    write('Enter the number of rows (n): '),
    read(N),
    write('Enter the number of columns (m): '),
    read(M).

create_grid(N, M, Grid) :-
    create_grid(0,N, M, [], Grid).

create_grid(N,N, _, Grid, Grid).
create_grid(CurrentRowNumber, N, M, Acc, Grid) :-
    create_row(CurrentRowNumber,0,M, [], Row),
    append(Acc, Row, NewAcc),
    NewRowNumber is CurrentRowNumber + 1,
    create_grid(NewRowNumber, N, M, NewAcc, Grid).

create_row(_, M, M, OutputRow, OutputRow).
create_row(CurrentRow, CurrentColumn, M, Acc, OutputRow) :-
    CurrentColumn < M,
    format('Enter a color for position (~w, ~w) (red/yellow/blue): ', [CurrentRow, CurrentColumn]),
    read(Color),
    (Color = red ; Color = yellow; Color = blue),
    append(Acc, [(CurrentRow,CurrentColumn,Color)], NewAcc),
    NewCurrentColumn is CurrentColumn + 1,
    create_row(CurrentRow, NewCurrentColumn, M, NewAcc, OutputRow).


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
    not(member(NextState, Parent)), % replace with not equal
    member(NextState, Grid),
    not(member(NextState, Visited)),
    append(Visited, [NextState], NewVisited),
    append(Path, [NextState], NewPath),
    search(NextState, Grid, NewVisited, NewPath, [CurrentState], Goal).

search(CurrentState, Grid, Visited, Path, Parent, Goal) :-
    move(CurrentState, Goal),
    not(member(Goal, Parent)), % replace with not equal
    member(Goal, Grid), % is a valid next state
    member(Goal, Visited),
	write("Search is complete!"), nl,
    write(Path).


search(_, _, _, _, _, _) :-
    write("No cycle found."), nl.


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





















