:- use_module(library(clpfd)).

% Andrew Myer 012939730
% CECS 424 Lab 1
% 9/27/18
%-------------------------------------------------------------
% sum_list([],Sum)
% finds the sum of the list by adding the head of the list and
% then iterating the next value in the list to be the head
% base case: the sum of the empty list is 0
% []:the list of cells to add
% Sum:the resulting sum of the list
get_value([X,Y],S,Z):-
        nth0(Y,S,Row),
        nth0(X,Row,Z).

sum_list([],S,0).
sum_list([Head|Tail],S,Sum) :-
        sum_list(Tail,S,Tsum),
        get_value(Head,S,Z),
        Sum #= Z + Tsum.
%-------------------------------------------------------------
% product_list([],product)
% multiplies all the values of the list together by multiplying
% the head of the list and iterating the next value as the value
% to multiply to the head.
% base case: with an empty list, although it doesnt make sense
% right now, for our purposes its 1
% []:the list of cells to multiply against eachother
% Product: the resulting product of the list
product_list([],S,1).
product_list([Head|Tail],S,Product) :-
        product_list(Tail,S,Tproduct),
        get_value(Head,S,Z),
        Product #= Z * Tproduct.
%--------------------------------------------------------------
% cell values
% assigns values to coordinates in a cell.
% base case: empty lists, do nothing
cell_values([],_,[]).
cell_values([[Coordheadx,Coordheady]|Coordtail],S, [Valuehead|Valuetail]):-
        nth0(Coordheady,S,Y),
        nth0(Coordheadx,Y,Valuehead),
        cell_values(Coordtail,S,Valuetail).
%--------------------------------------------------------------
% check constraint(cage(method, Value, Cells), S)
% checks if the value for the cage is the value of the cell
% cage: a box of cells that follows a specidied rule
% method: the method to which the cage will be checked to
% Value: the number to which the rule should result in
% Cells: the coordinates that the cage contains

% method = id:the cell in the cage will equal the Value
check_constraint(cage(id, Value, Cells), S):-
        cell_values(Cells,S,Value).
% method = add:the values of cells will all add up to the Value
check_constraint(cage(add, Value,Cells), S):-
        sum_list(Cells,S,X),
        X#=Value.
% method = sub:the values in the cells will subtract to the Value.
check_constraint(cage(sub, Value, [Cell1,Cell2]), S):-
        get_value(Cell1,S,C1),
        get_value(Cell2,S,C2),
        C1-C2#=Value;C2-C1#=Value.
% method = mult:values in the cells will multiply to Value
check_constraint(cage(mult, Value, Cells), S):-
        product_list(Cells,S,X),
        X#=Value.
% method = div: values in the cell will divide to Value
check_constraint(cage(div, Value, [Cell1,Cell2]), S):-
       get_value(Cell1,S,C1),
       get_value(Cell2,S,C2),
       X #= div(C1,C2),
       Y #= div(C2,C1),
       X #= Value; Y #= Value.
%---------------------------------------------------------------
%check_cages([Cages])
%uses check constraints to apply rules to all cages
%Cages: list of all cages in the game
check_cages([],_).
check_cages([CageHead|CageTail],S):-
       check_constraint(CageHead,S),
       check_cages(CageTail,S).
row_size([],_).
row_size([H|T],N):-
        length(H,N),
        row_size(T,N).
col_size(S,N):-
        length(S,N).
%---------------------------------------------------------------
kenken(Puzzle,Cages):-
        col_size(Puzzle,6),
        row_size(Puzzle,6),
        append(Puzzle, Values),
	Values ins 1..6,
        check_cages(Cages,Puzzle),
        maplist(all_different, Puzzle),
        transpose(Puzzle, Cols),
        maplist(all_different, Cols),
        maplist(label, Puzzle).

%Puzzle=[A,B,C,D,E,F],Cages=[cage(add,11,[[0,0],[0,1]]),cage(div,2,[[1,0],[2,0]]),cage(mult,20,[[3,0],[3,1]]),cage(mult,6,[[4,0],[5,0],[5,1],[5,2]]),cage(sub,3,[[1,1],[2,1]]),cage(div,3,[[4,1],[4,2]]),cage(mult,240,[[0,2],[0,3],[1,2],[1,3]]),cage(mult,6,[[2,2],[3,2]]),cage(mult,30,[[4,3],[5,3]]),cage(mult,6,[[2,3],[2,4]]),cage(add,7,[[3,3],[3,4],[4,4]]),cage(mult,6,[[0,4],[1,4]]),cage(add,8,[[0,5],[1,5],[2,5]]),cage(div,2,[[3,5],[4,5]]),cage(add,9,[[5,4],[5,5]])],kenken(Puzzle,Cages).
