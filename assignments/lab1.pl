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
product_list([],1).
product_list([Head|Tail],Product) :-
        product_list(Tail,Tproduct),
        Product #= Head * Tproduct.
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
        Cell1-Cell2#=Value;Cell2-Cell1#=Value.
% method = mult:values in the cells will multiply to Value
check_constraint(cage(mult, Value, Cells), S):-
        product_list(Cells,X),
        X#=Value.
% method = div: values in the cell will divide to Value
check_constraint(cage(div, Value, [Cell1,Cell2]), S):-
       X #= div(Cell1,Cell2),
       Y #= div(Cell2,Cell1),
       X #= Value; Y #= Value.
%---------------------------------------------------------------
%check_cages([Cages])
%uses check constraints to apply rules to all cages
%Cages: list of all cages in the game
check_cages([]).
check_cages([CageHead|CageTail]):-
        check_cages(CageTail),
        check_constraint(CageHead).

%---------------------------------------------------------------
kenken(Puzzle):-
        Puzzle=[A,B],
        A=[A1,A2],
        B=[B1,B2],
        Co1=[A1,B1],
        Co2=[A2,B2],
        A ins 1..2,
        B ins 1..2,
        %nth0(1,Puzzle,D),
        %nth0(1,D,E),
        %label(D).
        %cell_values([[1,1]],Puzzle,[2]),
        check_constraint(cage(add,4,[[1,1],[0,1]]),Puzzle),
        %sum_list([[1,1],[0,1]],Puzzle,Sum),
        Sum#=4,
        %all_different(A),
        %all_different(B),
        %all_different(Co1),
        %all_different(Co2),
        label(A),
        label(B).

% Puzzle=[[_,_],[_,_]],Puzzle=[A,B],kenken([A,B]).
