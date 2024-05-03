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
print_grid([Row|Rows]) :-
    print_row(Row, 1),
    nl,
    print_grid(Rows).

print_row([], _).
print_row([Color|Colors], Position) :-
    write('('),
    write(Color),
    write(') '),
    NextPosition is Position + 1,
    print_row(Colors, NextPosition).


% Finding the cycle
% Define valid moves within the grid bound
% Define the moves
move((X,Y,CurrentNodeColor), (NX, Y, CurrentNodeColor)) :- NX is X + 1. % Move down
move((X,Y,CurrentNodeColor), (NX, Y, CurrentNodeColor)) :- NX is X - 1. % Move up
move((X,Y,CurrentNodeColor), (X,NY,CurrentNodeColor)) :- NY is Y - 1. % Move left
%move([X, Y], [NX, Y]) :- NX is X - 1. % Move up

move((X,Y,CurrentNodeColor), (X, NY, CurrentNodeColor)) :- NY is Y + 1. % Move right
%move([X, Y], [X, NY]) :- NY is Y - 1. % Move left


% Depth-first search to find color cycle meeting specified conditions
search(CurrentState, Grid, Visited, Path, Parent) :-
    move(CurrentState, NextState),
    not(member(NextState, Parent)), % replace with not equal
    member(NextState, Grid), % is a valid next state
    member(NextState, Visited),
	write("Search is complete!"), nl,
    write(Path), nl.

search(CurrentState, Grid, Visited, Path, Parent) :-
    move(CurrentState, NextState),
    not(member(NextState, Parent)), % replace with not equal
    member(NextState, Grid),
    not(member(NextState, Visited)),
    append(Visited, [NextState], NewVisited),
    append(Path, [NextState], NewPath),
    search(NextState, Grid, NewVisited, NewPath, [CurrentState]).

trySearchStartPoint([],_).
trySearchStartPoint([FirstNode|Nodes], Grid) :-
    %write(Nodes), nl,
    trySearchStartPoint(Nodes, Grid),
    search(FirstNode, Grid, [FirstNode], [FirstNode], []).

%Main Predicate
main:-
    grid_size(N, M),
    create_grid(N, M, Grid),
    %print_grid(Grid),
    write('Done.'),
    trySearchStartPoint(Grid, Grid),
    writeln('Done.').
