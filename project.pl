size(20,20).

%walls
wall(point(1,2)).
wall(point(1,4)).
wall(point(1,6)).
wall(point(1,7)).
wall(point(1,15)).
wall(point(2,8)).
wall(point(2,11)).
wall(point(2,12)).
wall(point(2,13)).
wall(point(2,20)).
wall(point(3,5)).
wall(point(3,6)).
wall(point(3,7)).
wall(point(3,9)).
wall(point(3,10)).
wall(point(3,13)).
wall(point(4,4)).
wall(point(4,6)).
wall(point(4,17)).
wall(point(4,20)).
wall(point(5,8)).
wall(point(5,18)).
wall(point(6,1)).
wall(point(6,9)).
wall(point(6,17)).
wall(point(6,18)).
wall(point(7,12)).
wall(point(7,18)).
wall(point(7,20)).
wall(point(8,2)).
wall(point(8,3)).
wall(point(8,9)).
wall(point(8,12)).
wall(point(8,16)).
wall(point(8,19)).
wall(point(9,2)).
wall(point(9,7)).
wall(point(9,8)).
wall(point(9,11)).
wall(point(9,13)).
wall(point(9,15)).
wall(point(9,18)).
wall(point(10,2)).
wall(point(10,9)).
wall(point(10,18)).
wall(point(11,3)).
wall(point(11,12)).
wall(point(11,19)).
wall(point(12,3)).
wall(point(12,6)).
wall(point(12,8)).
wall(point(12,10)).
wall(point(12,13)).
wall(point(12,14)).
wall(point(12,19)).
wall(point(13,2)).
wall(point(13,5)).
wall(point(13,9)).
wall(point(13,12)).
wall(point(13,18)).
wall(point(13,19)).
wall(point(14,1)).
wall(point(14,3)).
wall(point(14,9)).
wall(point(15,1)).
wall(point(15,3)).
wall(point(15,4)).
wall(point(15,12)).
wall(point(15,20)).
wall(point(16,1)).
wall(point(16,3)).
wall(point(16,13)).
wall(point(17,1)).
wall(point(17,4)).
wall(point(17,15)).
wall(point(17,17)).
wall(point(18,8)).
wall(point(18,11)).
wall(point(18,12)).
wall(point(18,14)).
wall(point(18,15)).
wall(point(18,16)).
wall(point(19,1)).
wall(point(19,4)).
wall(point(19,8)).
wall(point(19,9)).
wall(point(19,10)).
wall(point(19,13)).
wall(point(20,6)).
wall(point(20,14)).
wall(point(20,15)).
wall(point(20,17)).
wall(point(20,19)).

%wall_num
wall_num(point(1,2),2).
wall_num(point(1,7),1).
wall_num(point(2,11),1).
wall_num(point(2,20),2).
wall_num(point(3,7),2).
wall_num(point(3,9),3).
wall_num(point(4,4),2).
wall_num(point(4,20),2).
wall_num(point(5,8),1).
wall_num(point(5,18),2).
wall_num(point(6,1),0).
wall_num(point(6,17),2).
wall_num(point(8,9),3).
wall_num(point(8,19),2).
wall_num(point(9,7),0).
wall_num(point(9,8),3).
wall_num(point(9,11),3).
wall_num(point(9,15),2).
wall_num(point(9,18),2).
wall_num(point(10,2),2).
wall_num(point(11,19),1).
wall_num(point(12,3),2).
wall_num(point(12,8),3).
wall_num(point(12,10),3).
wall_num(point(12,19),2).
wall_num(point(13,5),1).
wall_num(point(13,12),2).
wall_num(point(14,3),2).
wall_num(point(16,13),1).
wall_num(point(17,1),1).
wall_num(point(17,4),2).
wall_num(point(17,15),2).
wall_num(point(17,17),2).
wall_num(point(18,8),1).
wall_num(point(18,12),3).
wall_num(point(18,14),2).
wall_num(point(19,1),2).
wall_num(point(19,8),0).
wall_num(point(19,9),1).
wall_num(point(19,10),0).
wall_num(point(20,6),0).
wall_num(point(20,19),2).
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

%to check if a cell is lit (hit with some light
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

%helper function to return corners of the board.
return_list_corners(Result):- size(Rmax,Cmax), 
                                    append([point(1,1)],[],L1),
                                    append([point(Rmax,1)],[],L2),
                                    append([point(1,Cmax)],[],L3),
                                    append([point(Rmax,Cmax)],[],L4),
                                    append(L1,L2,R1),
                                    append(L3,L4,R2),
                                    append(R1,R2,Result).

% ---------------------------------------------------------------------------------------------------------------------
% we check if the cell's neighbor is not a wall or an edge or lit and then we add it to the list(returns only empty cells that are not lit).
%note: if the neighbor is a light we add it to the list, this solves situations like a three that has 3 neighbors and one of them is a light(we light up the 2 neighbors left).
right_neighbor(point(R,C),point(R,B)):- B is C+1,(not(lit(point(R,B)))),right_empty(point(R,C)),!.
right_neighbor(point(R,C),[]):- not(right_empty(point(R,C))),!.
right_neighbor(point(R,C),[]):- B is C+1,lit(point(R,B)).

left_neighbor(point(R,C),point(R,B)):- B is C-1,(not(lit(point(R,B)))),left_empty(point(R,C)),!.
left_neighbor(point(R,C),[]):- not(left_empty(point(R,C))),!.
left_neighbor(point(R,C),[]):- B is C-1, lit(point(R,B)).

up_neighbor(point(R,C),point(A,C)):- A is R-1,(not(lit(point(A,C)))),up_empty(point(R,C)),!.
up_neighbor(point(R,C),[]):- not(up_empty(point(R,C))),!.
up_neighbor(point(R,C),[]):- A is R-1, lit(point(A,C)).

down_neighbor(point(R,C),point(A,C)):- A is R+1, (not(lit(point(A,C)))),down_empty(point(R,C)),!.
down_neighbor(point(R,C),[]):- not(down_empty(point(R,C))),!.
down_neighbor(point(R,C),[]):- A is R+1, lit(point(A,C)).

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
%helper functions to return a row, a column of a full board
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

%helper function to return the type of each point in the board (wall,wallnum, etc..).
return_all_points_type(AllPointsType):- return_all_points(AllPoints),return_all_points_type(AllPoints,AllPointsType).

return_all_points_type([point(R,C)|T1],[wall(point(R,C))|T2]):- wall(point(R,C)),return_all_points_type(T1,T2),!.
return_all_points_type([point(R,C)|T1],[wall_num(point(R,C),_)|T2]):- wall_num(point(R,C),_),return_all_points_type(T1,T2),!.
return_all_points_type([point(R,C)|T1],[light(point(R,C))|T2]):- light(point(R,C)),return_all_points_type(T1,T2),!.


cell(point(R,C)):- return_all_points(Result),member(point(R,C),Result).

%to check if all cells in the board are lit.
all_cells_lit:- return_all_points(Result),all_cells_lit(Result).
all_cells_lit([H|T]):- lit(H),all_cells_lit(T),!.
all_cells_lit([H|T]):- (wall(H);wall_num(H,_)),all_cells_lit(T),!.
all_cells_lit([]):-!.
%-----------------------------------------------------------
solved:- all_cells_lit,no_double_light,light_count_correct.

%--------------------------------------------------SOLUTION SECTION------------------------------------------------------------------------------













%-----------------------------------------------------------------------------------------------------------------------------------------------
%defining our dynamic variables
:-dynamic light/1.
:-dynamic nolight/1.

% clear all dynamic facts because facts are stored in the current
% session so clear_all_lights or clear_all_no_lights must be called to
% clear previous stored dynamic facts
clear_all_lights:- retractall(light(_)).
clear_all_nolights:-retractall(nolight(_)).
clear:-clear_all_lights,clear_all_nolights.
%-----------------------------------------------------------------------------------------------------------------------------------------------










%print board functions.
return_full_row_type(R,AllPointsType,_):- return_full_row(R,AllPoints),return_full_row_type(AllPoints,AllPointsType).

return_full_row_type([point(R,C)|T1],[wall_num(point(R,C),N)|T2]):- wall_num(point(R,C),N),return_full_row_type(T1,T2),!.
return_full_row_type([point(R,C)|T1],[wall(point(R,C))|T2]):- wall(point(R,C)),return_full_row_type(T1,T2),!.
return_full_row_type([point(R,C)|T1],[light(point(R,C))|T2]):- light(point(R,C)),return_full_row_type(T1,T2),!.
return_full_row_type([point(R,C)|T1],[lit(point(R,C))|T2]):- lit(point(R,C)),return_full_row_type(T1,T2),!.
return_full_row_type([point(R,C)|T1],[point(R,C)|T2]):- return_full_row_type(T1,T2),!.

return_full_row_type([point(R,C)],[wall_num(point(R,C),N)]):- wall_num(point(R,C),N),!.
return_full_row_type([point(R,C)],[wall(point(R,C))]):- wall(point(R,C)),!.
return_full_row_type([point(R,C)],[light(point(R,C))]):- light(point(R,C)),!.
return_full_row_type([point(R,C)],[lit(point(R,C))]):- lit(point(R,C)),!.
return_full_row_type([point(R,C)],[point(R,C)]):-!.

print_row(R,_,_):- return_full_row_type(R,Board,2),print_row(Board).

print_row([wall_num(point(_,_),N)]):-   write(N)  ,!.
print_row([wall(point(_,_))]):-         write('#'),!.
print_row([light(point(_,_))]):-        write('o'),!.
print_row([lit(point(_,_))]):-          write('x'),!.
print_row([point(_,_)]):-               write('-'),!.

print_row([wall_num(point(_,_),N)|T]):- write(N),   write(' '),print_row(T),!.
print_row([wall(point(_,_))|T]):-       write('#'), write(' '),print_row(T),!.
print_row([light(point(_,_))|T]):-      write('o'), write(' '),print_row(T),!.
print_row([lit(point(_,_))|T]):-        write('x'), write(' '),print_row(T),!.
print_row([point(_,_)|T]):-             write('-'), write(' '),print_row(T),!.

print_board:- size(_,Cmax),print_board(1,Cmax).
print_board(R,Cmax):- R>Cmax,!.
print_board(R,Cmax):- print_row(R,_,_),write('\n'),R1 is R+1, print_board(R1,Cmax).

%-----------------------------------------------------------------------------------------------------------
%to put lights in obvious spots such as wallnum(4) and wallnum(3) that has only 3 neighbors for example.
%note: this also solves situations like a three that has 3 neighbors and one of them is a light(we light up the 2 neighbors left).
light_up_obvious_neighbors(point(R,C)):-return_num(point(R,C),Num),
                                        wall_num(point(R,C),Num), 
                                        light_neighbors(point(R,C),Lights),
                                        count(Lights,NumLights),
                                        Num is NumLights,
                                        !, fail.

light_up_obvious_neighbors(point(R,C)):-return_num(point(R,C),Num),
                                        wall_num(point(R,C),Num), 
                                        neighbors(point(R,C),List),
                                        count(List,Num),
                                        light_up_obvious_neighbors(List),!.

light_up_obvious_neighbors(point(R,C)):-return_num(point(R,C),WallNum),
                                        wall_num(point(R,C),WallNum),
                                        light_neighbors(point(R,C),Lights),
                                        count(Lights,NumLights),
                                        neighbors(point(R,C),List),
                                        count(List,NumNeighbors),
                                        WallNum is NumNeighbors+NumLights,
                                        light_up_obvious_neighbors(List),!.

light_up_obvious_neighbors([point(R,C)|T]):-not(lit(point(R,C))),assertz(light(point(R,C))),light_up_obvious_neighbors(T),!.
light_up_obvious_neighbors([point(R,C)|T]):-lit(point(R,C)),light(point(R,C)),assertz(light(point(R,C))),light_up_obvious_neighbors(T),!.
light_up_obvious_neighbors([[]|T]):-light_up_obvious_neighbors(T),!.
light_up_obvious_neighbors([]):-!.

%to light up all obvious wallnums in the board.


light_up_all_obvious(Status):- return_list_wall_num(List), light_up_all_obvious(List,Status).
light_up_all_obvious([point(R,C)|T],[1|T1]):-light_up_obvious_neighbors(point(R,C)),light_up_all_obvious(T,T1),!.
light_up_all_obvious([point(R,C)|T],[[]|T1]):- not(light_up_obvious_neighbors(point(R,C))),light_up_all_obvious(T,T1),!.
light_up_all_obvious([],[]):-print_board,!.

%helper function to clear, consult, and print quickly
cc():- consult('project.pl'),clear().
c():- consult('project.pl').
p():-print_board.
s():-set_prolog_flag(answer_write_options,[max_depth(0)]).

%function to keep solving all obvious wall nums repeatedly until no more obvious wall nums are found.
% we store all results (true or false) in a list and keep looping through this function until there is no true in the list
% which means that the function cannot light up any abvious wallnums because there aren't any left.
solve_all_obvious():-write('\n///////////////Checking for Obvious Wallnums to Solve (First Pass)//////////////////// \n\n'),
                     light_up_all_obvious(List),count(List,Num),Num>0,solve_all_obvious(Num).


solve_all_obvious(0):- write('\n///////////////All Obvious Wallnums Are Solved !//////////////////// \n\n'),!.

solve_all_obvious(_):-write('\n///////////////New Pass to Check for Obvious Wallnums//////////////////// \n\n'),
                        light_up_all_obvious(List),count(List,Num),solve_all_obvious(Num).



%function that lights up all empty cells that has no neighbors (not empty,lit cells around them)(isolated empty cells).
solve_isolated_cells():-write('\n///////////////Checking For Isolated Cells To Solve//////////////////// \n\n'),
                       return_all_points(List), solve_isolated_cells(List).

solve_isolated_cells([H|T]):- not(lit(H)),
                              neighbors(H,List),count(List,0),
                              assertz(light(H)),
                              print_board(),write('\n'),
                              solve_isolated_cells(T),!.

solve_isolated_cells([_|T]):- solve_isolated_cells(T).

solve_isolated_cells([]):- print_board(),write('\n'),
                           write('\n///////////////All Isolated Cells Are Checked & Solved !//////////////////// \n\n'),!.

%final function to solve the board.
solve:-clear(),solve_all_obvious(),solve_isolated_cells(),
       write('\n///////////////Puzzle Solved !//////////////////// \n\n').


%-----------------------------------------------------------------END OF PROJECT-------------------------------------------------------------------------------