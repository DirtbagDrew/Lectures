:- use_module(library(clpfd)).

%-------------------------------------------------------------
% sum_list
% finds the sum of the list by adding the head of the list and
% then iterating the next value in the list to be the head
% base case: the sum of the empty list is 0
sum_list([],0).
sum_list([Head|Tail],Sum) :-
        sum_list(Tail, Tsum),
        Sum is Head + Tsum.
%-------------------------------------------------------------
% product_list
% multiplies all the values of the list together by multiplying
% the head of the list and iterating the next value as the value
% to multiply to the head.
% base case: with an empty list, although it doesnt make sense
% right now, for our purposes its 1
product_list([],1).
product_list([Head|Tail],Product) :-
        product_list(Tail,Tproduct),
        Product is Head * Tproduct.
%--------------------------------------------------------------
% cell values
% assigns values to coordinates in a cell.
% base case: empty lists, do nothing
cell_values([],[]).
cell_values([Coordhead|Coordtail], [Valuehead|Valuetail]):-
        Coordhead = Valuehead,
        cell_values(Coordtail,Valuetail).
%--------------------------------------------------------------
check_constraint(cage(add, Value, Cells), S).
check_constraint(cage(sub, Value, Cells), S).
check_constraint(cage(mult, Value, Cells), S).
check_constraint(cage(div, Value, Cells), S).
check_constraint(cage(id, Value, Cells), S).

