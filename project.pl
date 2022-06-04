size(8,8).

%walls
wall(point(1,6)).
wall(point(2,2)).
wall(point(2,3)).
wall(point(3,7)).
wall(point(4,1)).
wall(point(4,5)).
wall(point(5,4)).
wall(point(5,8)).
wall(point(6,2)).
wall(point(7,6)).
wall(point(7,7)).
wall(point(8,3)).


%wall_num
wall_num(point(1,6),1).
wall_num(point(2,2),3).
wall_num(point(3,7),0).
wall_num(point(5,4),4).
wall_num(point(5,8),0).
wall_num(point(6,2),2).
wall_num(point(7,6),1).

%lights
light(point(1,2)).
light(point(1,7)).
light(point(2,1)).
light(point(2,8)).
light(point(3,2)).
light(point(4,4)).
light(point(4,6)).
light(point(5,3)).
light(point(5,5)).
light(point(6,1)).
light(point(6,4)).
light(point(7,2)).
light(point(7,8)).
light(point(8,6)).


%rules

%-------------------------------------------TESTING SECTION----------------------------------------------------------------------------------
%to check if a point is inside the boudaries of the board
inside_bounds(point(R,C)):- size(Rmax,Cmax),R > 0,C > 0,R=<Rmax,C=<Cmax.

%to check if a point is not a wall
no_wall(point(R,C)):- not(wall(point(R,C))), inside_bounds(point(R,C)).

%to check if a point is not a wall_num
no_wall_num(point(R,C),_):- not(wall_num(point(R,C),_)), inside_bounds(point(R,C)).

%to check if the right neighbour of a point is not a wall
right_empty(point(R,C)):- size(_,Cmax), inside_bounds(point(R,C)),
                    C1 is C + 1 , C1 =< Cmax ,
                    no_wall(point(R,C1)) , no_wall_num(point(R,C1),_).

%to check if the left neighbour of a point is not a wall
left_empty(point(R,C)):- inside_bounds(point(R,C)),
                    C1 is C - 1 , C1 > 0,
                    no_wall(point(R,C1)) , no_wall_num(point(R,C1),_).

%to check if the top neighbour of a point is not a wall
up_empty(point(R,C)):- inside_bounds(point(R,C)),
                    R1 is R - 1 ,R1 > 0,
                    no_wall(point(R1,C)) , no_wall_num(point(R1,C),_).

%to check if the bottom neighbour of a point is not a wall
down_empty(point(R,C)):- size(Rmax,_),inside_bounds(point(R,C)),
                    R1 is R + 1 ,R1 =< Rmax,
                    no_wall(point(R1,C)) , no_wall_num(point(R1,C),_).


%to check if four sides empty and num is four then it is ready to be lit
sides_empty(R,C):- right_empty(point(R,C)),left_empty(point(R,C)),up_empty(point(R,C)),down_empty(point(R,C)).

%return list with all elements to the right of the cell until it hits a wall or the edge
return_list_right(point(R,C),[point(R,C)]):- not(right_empty(point(R,C))) ,!.
return_list_right(point(R,C),[point(R,C)|T]):- right_empty(point(R,C)), C1 is C+1 , return_list_right(point(R,C1),T) .

%return list with all elements to the left of the cell until it hits a wall or the edge
return_list_left(point(R,C),[point(R,C)]):- not(left_empty(point(R,C))) ,!.
return_list_left(point(R,C),[point(R,C)|T]):- left_empty(point(R,C)), C1 is C-1 , return_list_left(point(R,C1),T).

%return list with all elements up of the cell until it hits a wall or the edge
return_list_up(point(R,C),[point(R,C)]):- not(up_empty(point(R,C))) ,!.
return_list_up(point(R,C),[point(R,C)|T]):- up_empty(point(R,C)), R1 is R-1 , return_list_up(point(R1,C),T).

%return list with all elements down of the cell until it hits a wall or the edge
return_list_down(point(R,C),[point(R,C)]):- not(down_empty(point(R,C))) ,!.
return_list_down(point(R,C),[point(R,C)|T]):- down_empty(point(R,C)), R1 is R+1 , return_list_down(point(R1,C),T).

%note this function returns the same cell in the two lists example: [[point(1,2),point(1,1)],[point(1,2),point(1,3),point(1,4)]]
%this function returns the same row without checking if it is lighted by another light.
return_list_row(point(R,C),[LEFT,RIGHT]):- return_list_left(point(R,C),LEFT),return_list_right(point(R,C),RIGHT).

%this function returns the same row without checking if it is lighted by another light.
%note this function returns the same cell in the two lists example: [[point(1,2),point(1,1)],[point(1,2),point(1,3),point(1,4)]]
return_list_col(point(R,C),[UP,DOWN]):- return_list_up(point(R,C),UP),return_list_down(point(R,C),DOWN).

return_list_row_col(point(R,C),[LEFT,RIGHT,UP,DOWN]):- return_list_left(point(R,C),LEFT),return_list_right(point(R,C),RIGHT),return_list_up(point(R,C),UP),return_list_down(point(R,C),DOWN).

lit(point(R,C)):- (light(point(R,C)) ,!);
                    ((return_list_row_col(point(R,C),[LEFT,RIGHT,UP,DOWN])),
                    ((lit(point(R,C),LEFT),!); (lit(point(R,C),RIGHT),!); (lit(point(R,C),UP),!); (lit(point(R,C),DOWN),!))).
lit(point(_,_),[H|T]):- light(H),! ; lit(point(_,_),T).

%return a list of all walls in the board
return_list_wall(Result):- findall(point(R,C),wall(point(R,C)),Result).
%returns a list of all numbered walls in a list
return_list_wall_num(Result):- findall(point(R,C),wall_num(point(R,C),_),Result).
%returns a list of all the lights in a board
return_list_light(Result):- findall(point(R,C),light(point(R,C)),Result).



% ---------------------------------------------------------------------------------------------------------------------
% we check if the cell's neighbor is not a wall or an edge and then we add it to the list.
right_neighbor(point(R,C),point(R,B)):- right_empty(point(R,C)), B is C+1,!.
right_neighbor(point(R,C),[]):- not(right_empty(point(R,C))).

left_neighbor(point(R,C),point(R,B)):- left_empty(point(R,C)), B is C-1,!.
left_neighbor(point(R,C),[]):- not(left_empty(point(R,C))).

up_neighbor(point(R,C),point(A,C)):- up_empty(point(R,C)), A is R-1,!.
up_neighbor(point(R,C),[]):- not(up_empty(point(R,C))).

down_neighbor(point(R,C),point(A,C)):- down_empty(point(R,C)), A is R+1,!.
down_neighbor(point(R,C),[]):- not(down_empty(point(R,C))).

%to return a list of a cell's neighbors
neighbors(point(R,C),[RIGHT,LEFT,UP,DOWN]):- right_neighbor(point(R,C),RIGHT),
                                             left_neighbor(point(R,C),LEFT),
                                             up_neighbor(point(R,C),UP),
                                             down_neighbor(point(R,C),DOWN).




% we check if the cell's neighbor is not a wall or an edge [[[[and it is a light]]]]then we add it to the list.
right_light_neighbor(point(R,C),[]):- not(right_empty(point(R,C))),!; (B is C+1,not(light(point(R,B)))),!.
right_light_neighbor(point(R,C),point(R,B)):- light(point(R,B)), right_empty(point(R,C)), B is C+1,!.

left_light_neighbor(point(R,C),[]):- not(left_empty(point(R,C))),!; (B is C-1,not(light(point(R,B)))),!.
left_light_neighbor(point(R,C),point(R,B)):- light(point(R,B)), left_empty(point(R,C)), B is C-1,!.

up_light_neighbor(point(R,C),[]):- not(up_empty(point(R,C))),!; (A is R-1,not(light(point(A,C)))),!.
up_light_neighbor(point(R,C),point(A,C)):- light(point(A,C)), up_empty(point(R,C)), A is R-1,!.

down_light_neighbor(point(R,C),[]):- not(down_empty(point(R,C))),!; (A is R+1,not(light(point(A,C)))),!.
down_light_neighbor(point(R,C),point(A,C)):- light(point(A,C)), down_empty(point(R,C)), A is R+1,!.

%to return a list of a cell's neighbors that are lights
light_neighbors(point(R,C),[RIGHT,LEFT,UP,DOWN]):- right_light_neighbor(point(R,C),RIGHT),
                                             left_light_neighbor(point(R,C),LEFT),
                                             up_light_neighbor(point(R,C),UP),
                                             down_light_neighbor(point(R,C),DOWN).



%check if wall_num cell has lights equal to its number
%helper function to count the number of elements in a list and ignore empty lists
count([],0).
count([H|T], N) :- count(T, N1), ((H\=[])-> N is N1 + 1 ; N = N1).
%helper function to return the number in a wall num cell
return_num(point(R,C),Num):- wall_num(point(R,C),Num).
return_num(point(R,C),0):- not(wall_num(point(R,C),_)).
%the actual function
wall_num_check_lights(point(R,C)):- return_num(point(R,C),Num),wall_num(point(R,C),Num),light_neighbors(point(R,C),X),count(X,Num).
%to check lights around wall nums in all the board
light_count_correct:- return_list_wall_num(Result),light_count_correct(Result),!.
light_count_correct([point(R,C)|T]):- wall_num_check_lights(point(R,C)),light_count_correct(T).
light_count_correct([]):-!.



%--------------------------------------------------------------------------------------------------------------------------------
%counts lights in a list
light_cells_count([],0).
light_cells_count([H|T], LightCells):- light(H),light_cells_count(T, LightCells1), LightCells is LightCells1 + 1.
light_cells_count([H|T], LightCells):- not(light(H)),light_cells_count(T,LightCells).

remove_duplicates([],[]).
remove_duplicates([H | T], List) :- member(H, T),remove_duplicates( T, List).
remove_duplicates([H | T], [H|T1]) :- \+member(H, T),remove_duplicates( T, T1).
%double light in row check
no_double_light_row:-return_all_points(Result),no_double_light_row(Result).
no_double_light_row([H|T]):-not(wall(H)),return_list_row(H,Row),flatten(Row,RowList),
 remove_duplicates(RowList,FinalRow),light_cells_count(FinalRow,
 LightCells),
LightCells=<1,no_double_light_row(T),!.
no_double_light_row([H|T]):-wall(H),no_double_light_row(T),!.
no_double_light_row([]):-!.


no_double_light_col:-return_all_points(Result),no_double_light_col(Result).
no_double_light_col([H|T]):-not(wall(H)),return_list_col(H,Col),flatten(Col,ColList),
 remove_duplicates(ColList,FinalCol),light_cells_count(FinalCol,
 LightCells),
LightCells=<1,no_double_light_col(T),!.
no_double_light_col([H|T]):-wall(H),no_double_light_col(T),!.
no_double_light_col([]):-!.
no_double_light:-no_double_light_col,no_double_light_row.
%--------------------------------------------------------------------------------------------------------------------------------
return_full_row(R,[point(R,1)|T]):- return_full_row(R,2,T) .
return_full_row(R,C,[point(R,C)|T]):- C1 is C+1 ,inside_bounds(point(R,C1)), return_full_row(R,C1,T),!.
return_full_row(R,C,[point(R,C)]):- !.

return_full_col(C,[point(1,C)|T]):- return_full_col(2,C,T) .
return_full_col(R,C,[point(R,C)|T]):- R1 is R + 1 ,inside_bounds(point(R1,C)), return_full_col(R1,C,T),!.
return_full_col(R,C,[point(R,C)]):- !.

return_all_points(Result):- return_full_col(1,TheCol),return_all_points(Result,TheCol,[]).
return_all_points(Result,[point(K,_)|T],[]):- return_full_row(K,TheRow), return_all_points(Result,T,TheRow),!.
return_all_points(Result,[point(K,_)|T],Acc):- return_full_row(K,TheRow) , append(Acc,TheRow,NewAcc),return_all_points(Result,T,NewAcc),!.
return_all_points(NewAcc,[point(K,_)],Acc):- return_full_row(K,TheRow) , append(Acc,TheRow,NewAcc).

return_all_points_type(AllPointsType):- return_all_points(AllPoints),return_all_points_type(AllPoints,AllPointsType).

return_all_points_type([point(R,C)|T1],[wall_num(point(R,C),N)|T2]):- wall_num(point(R,C),N),return_all_points_type(T1,T2),!.
return_all_points_type([point(R,C)|T1],[wall(point(R,C))|T2]):- wall(point(R,C)),return_all_points_type(T1,T2),!.
return_all_points_type([point(R,C)|T1],[light(point(R,C))|T2]):- light(point(R,C)),return_all_points_type(T1,T2),!.
return_all_points_type([point(R,C)|T1],[point(R,C)|T2]):- return_all_points_type(T1,T2),!.

return_all_points_type([point(R,C)],[wall(point(R,C))]):- wall(point(R,C)),!.
return_all_points_type([point(R,C)],[wall_num(point(R,C),_)]):- wall_num(point(R,C),_),!.
return_all_points_type([point(R,C)],[light(point(R,C))]):- light(point(R,C)),!.
return_all_points_type([point(R,C)],[point(R,C)]).

cell(point(R,C)):- return_all_points(Result),member(point(R,C),Result).

all_cells_lit:- return_all_points(Result),all_cells_lit(Result),!.
all_cells_lit([H|T]):- lit(H),all_cells_lit(T).
all_cells_lit([H]):- lit(H).
%-----------------------------------------------------------
solved:- all_cells_lit,no_double_light,light_count_correct.

%--------------------------------------------------SOLUTION SECTION------------------------------------------------------------------------------


%nigoghosian's code=====================================================================================================
%defining our dynamic variables
:-dynamic light/1.
:-dynamic nolight/1.

% clear all dynamic facts because facts are stored in the current
% session so clear_all_lights or clear_all_no_lights must be called to
% clear previous stored dynamic facts
clear_all_lights:- retractall(light(_)).
clear_all_nolights:-retractall(nolight(_)).
clear:-clear_all_lights,clear_all_nolights.

%functions to light up the cell containing number four from all sides
light_up_number_four_top(point(R,C)):-up_empty(point(R,C)),!,R1 is R-1,assertz(light(point(R1,C))),!.
light_up_number_four_bottom(point(R,C)):-down_empty(point(R,C)),!,R2 is R+1,assertz(light(point(R2,C))),!.
light_up_number_four_left(point(R,C)):-left_empty(point(R,C)),!,C1 is C-1,assertz(light(point(R,C1))),!.
light_up_number_four_right(point(R,C)):-right_empty(point(R,C)),!,C2 is C+1,assertz(light(point(R,C2))),!.

light_up_number_four(point(R,C)):-light_up_number_four_top(point(R,C)),light_up_number_four_bottom(point(R,C)),light_up_number_four_right(point(R,C)),light_up_number_four_left(point(R,C)).

%function to return wall numbers with number 4
return_list_wall_num_with_fours(Result):- findall(point(R,C),wall_num(point(R,C),4),Result).

%find all fours in the grid and light them up
light_up_all_fours:-return_list_wall_num_with_fours(L),light_up_all_fours1(L),!.
light_up_all_fours1([H|T]):-light_up_number_four(H),light_up_all_fours1(T).
light_up_all_fours1([]):-!.

% functions to disable light from cell containing number zero from four
% sides
disable_light_number_zero_top(point(R,C)):-R1 is R-1,not(inside_bounds(point(R1,C))),!;R1 is R-1,assertz(nolight(point(R1,C))),!.
disable_light_number_zero_bottom(point(R,C)):-R2 is R+1,not(inside_bounds(point(R2,C))),!;R2 is R+1,assertz(nolight(point(R2,C))),!.
disable_light_number_zero_left(point(R,C)):-C1 is C-1,not(inside_bounds(point(R,C1))),!;C1 is C-1,assertz(nolight(point(R,C1))),!.
disable_light_number_zero_right(point(R,C)):-C2 is C+1,not(inside_bounds(point(R,C2))),!;C2 is C+1,assertz(nolight(point(R,C2))),!.

disable_light_number_zero(point(R,C)):-disable_light_number_zero_top(point(R,C)),disable_light_number_zero_bottom(point(R,C)),disable_light_number_zero_left(point(R,C)),disable_light_number_zero_right(point(R,C)).

%johny's code=====================================================================================================
%axomatics
%count list elements
count_e([],0).
count_e([_|T],N):-count_e(T,N1),N is N1+1.
%count neighbors list elements
neighbors_count(X,Y,Z):-neighbors(point(X,Y),Elements),flatten(Elements,F_Elements),count_e(F_Elements,Z).
%return the number in the wall
return_wall_num(X,Y,Z):-wall_num(point(X,Y),Z).
%check if number of the wall is 4
check_wall_num_4(X,Y):-return_wall_num(X,Y,Z),Z = 4.
%check if number of the wall is 0
check_wall_num_0(X,Y):-return_wall_num(X,Y,Z),Z = 0.
%check if number of the wall is 2
check_wall_num_2(X,Y):-return_wall_num(X,Y,Z),Z = 2.
%check if number of the wall is 3
check_wall_num_3(X,Y):-return_wall_num(X,Y,Z),Z = 3.
%check if the cell in the corner
check_corner(X,Y):-neighbors_count(X,Y,Z),Z = 2.
%check if the cell in the edge
check_edge(X,Y):-neighbors_count(X,Y,Z),Z = 3.

%nicolas's code=====================================================================================================
return_full_row_type(R,AllPointsType,_):- return_full_row(R,AllPoints),return_full_row_type(AllPoints,AllPointsType).

return_full_row_type([point(R,C)|T1],[wall_num(point(R,C),N)|T2]):- wall_num(point(R,C),N),return_full_row_type(T1,T2),!.
return_full_row_type([point(R,C)|T1],[wall(point(R,C))|T2]):- wall(point(R,C)),return_full_row_type(T1,T2),!.
return_full_row_type([point(R,C)|T1],[light(point(R,C))|T2]):- light(point(R,C)),return_full_row_type(T1,T2),!.
return_full_row_type([point(R,C)|T1],[lit(point(R,C))|T2]):- lit(point(R,C)),return_full_row_type(T1,T2),!.
return_full_row_type([point(R,C)|T1],[point(R,C)|T2]):- return_full_row_type(T1,T2),!.

return_full_row_type([point(R,C)],[wall(point(R,C))]):- wall(point(R,C)),!.
return_full_row_type([point(R,C)],[wall_num(point(R,C),_)]):- wall_num(point(R,C),_),!.
return_full_row_type([point(R,C)],[light(point(R,C))]):- light(point(R,C)),!.
return_full_row_type([point(R,C)],[lit(point(R,C))]):- lit(point(R,C)),!.
return_full_row_type([point(R,C)],[point(R,C)]).

print_row(R,_,_):- return_full_row_type(R,Board,2),print_row(Board).

print_row([wall_num(point(_,_),N)|T]):- write(N),write(' '),print_row(T),!.
print_row([wall(point(_,_))|T]):- write('#'),write(' '),print_row(T),!.
print_row([light(point(_,_))|T]):- write('O'),write(' '),print_row(T),!.
print_row([lit(point(_,_))|T]):- write('X'),write(' '),print_row(T),!.
print_row([point(_,_)|T]):- write('_'),write(' '),print_row(T),!.

print_row([wall_num(point(_,_),N)]):- write(N),!.
print_row([wall(point(_,_))]):- write('#'),!.
print_row([light(point(_,_))]):- write('O'),!.
print_row([lit(point(_,_))]):- write('X'),!.
print_row([point(_,_)]):- write('_'),!.

print_board:- size(_,Cmax),print_board(1,Cmax).
print_board(R,Cmax):- R>Cmax,!.
print_board(R,Cmax):- print_row(R,_,_),write('\n'),R1 is R+1, print_board(R1,Cmax).