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
no_wall(point(R,C)):- not(wall(point(R,C))).
no_wall_num(point(R,C),_):- not(wall_num(point(R,C),_)).
inside_bounds(R,C):- size(Rmax,Cmax),R > 0,C > 0,R=<Rmax,C=<Cmax.

right_empty(R,C):-size(_,Cmax), inside_bounds(R,C),
                    C1 is C + 1 , C1 =< Cmax ,
                    no_wall(point(R,C1)) , no_wall_num(point(R,C1),_).
left_empty(R,C):- inside_bounds(R,C),
                    C1 is C - 1 , C1 > 0, 
                    no_wall(point(R,C1)) , no_wall_num(point(R,C1),_).
top_empty(R,C):- inside_bounds(R,C), 
                    R1 is R - 1 ,R1 > 0, 
                    no_wall(point(R1,C)) , no_wall_num(point(R1,C),_).
down_empty(R,C):- size(Rmax,_),inside_bounds(R,C), 
                    R1 is R + 1 ,R1 =< Rmax, 
                    no_wall(point(R1,C)) , no_wall_num(point(R1,C),_).


%check if four sides empty and num is four then it is ready to be lit
sides_empty(R,c):- right_empty(R,C),left_empty(R,C),top_empty(R,C),down_empty(R,C).


return_list_right(R,C,[R,C|T]):- right_empty(R,C), C1 is C+1 , return_list_right(R,C1,T) .
return_list_right(R,C,[R,C|T]):- not(right_empty(R,C)).


%this function returns the same row without checking if it is lighted by another light
%same_row(point(R,C),[point(R,C) |T]):- 