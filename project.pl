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



% george's code---------------------------------------------------------------------------------------------------------------------
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

left_light_neighbor(point(R,C),point(R,B)):- light(point(R,B)), left_empty(point(R,C)), B is C-1,!.
left_light_neighbor(point(R,C),[]):- not(left_empty(point(R,C))); (not(light(point(R,B))),B is C-1).

up_light_neighbor(point(R,C),point(A,C)):- light(point(A,C)), up_empty(point(R,C)), A is R-1,!.
up_light_neighbor(point(R,C),[]):- not(up_empty(point(R,C))); (not(light(point(A,C))),A is R-1).

down_light_neighbor(point(R,C),point(A,C)):- light(point(A,C)), down_empty(point(R,C)), A is R+1,!.
down_light_neighbor(point(R,C),[]):- not(down_empty(point(R,C))); (not(light(point(A,C))),A is R+1).

%to return a list of a cell's neighbors that are lights
light_neighbors(point(R,C),[RIGHT,LEFT,UP,DOWN]):- right_light_neighbor(point(R,C),RIGHT),
                                             left_light_neighbor(point(R,C),LEFT),
                                             up_light_neighbor(point(R,C),UP),
                                             down_light_neighbor(point(R,C),DOWN).



%check if wall_num cell has lights equal to its number
%helper function to count the number of elements in a list and ignore empty lists
count([],0).
count([H|T], N) :- count(T, N1), ((H\=[])-> N is N1 + 1 ; N = N1).
%the actual function
wall_num_check_lights(point(R,C),Num):- wall_num(point(R,C),Num), light_neighbors(point(R,C),X),count(X,Num).


%fix not light==> when there is no light it returns false,we need to t=return empty, otherwise it is working(it detects lights and walls and edges)
%all functions are working except for not light (eza btm7i shi naykak)

%rita's code--------------------------------------------------------------------------------------------------------------------------------
%counts lights in a list
light_cells_count([],0).
light_cells_count([H|T], LightCells):- light(H),light_cells_count(T, LightCells1), LightCells is LightCells1 + 1.
light_cells_count([H|T], LightCells):- not(light(H)),light_cells_count(T,LightCells).

%double light in row check
no_double_light_row(point(R,C)):- return_list_row(point(R,C),List),flatten(List,List2),light_cells_count(List2, LightCells),LightCells=<1.
%double light in column check
no_double_light_col(point(R,C)):- return_list_col(point(R,C),List),flatten(List,List2),light_cells_count(List2, LightCells),LightCells=<1.
%double light check
no_double_light(point(R,C)):-no_double_light_row(point(R,C)),no_double_light_col(point(R,C)).



%nicolas's code--------------------------------------------------------------------------------------------------------------------------------
return_full_row(R,[point(R,1)|T]):- return_full_row(R,2,T) .
return_full_row(R,C,[point(R,C)|T]):- C1 is C+1 ,inside_bounds(point(R,C1)), return_full_row(R,C1,T),!.
return_full_row(R,C,[point(R,C)]):- !.

return_full_col(C,[point(1,C)|T]):- return_full_col(2,C,T) .
return_full_col(R,C,[point(R,C)|T]):- R1 is R + 1 ,inside_bounds(point(R1,C)), return_full_col(R1,C,T),!.
return_full_col(R,C,[point(R,C)]):- !.

return_all_points(Result):- return_full_col(1,TheCol),return_all_points(Result,TheCol,[]).
return_all_points(Result,[point(Khara,_)|T],[]):- return_full_row(Khara,TheRow), return_all_points(Result,T,TheRow),!.
return_all_points(Result,[point(Khara,_)|T],Acc):- return_full_row(Khara,TheRow) , append(Acc,TheRow,NewAcc),return_all_points(Result,T,NewAcc),!.
return_all_points(NewAcc,[point(Khara,_)],Acc):- return_full_row(Khara,TheRow) , append(Acc,TheRow,NewAcc).

cell(point(R,C)):- return_all_points(Result),member(point(R,C),Result).

all_cells_lit:- return_all_points(Result),all_cells_lit(Result),!.
all_cells_lit([H|T]):- lit(H),all_cells_lit(T).
all_cells_lit([H]):- lit(H).