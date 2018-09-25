:- use_module(library(clpfd)).

% Andrew Myer 012939730
% CECS 424 Lab 1
% 9/27/18
%-------------------------------------------------------------
% sum_list
% finds the sum of the list by adding the head of the list and
% then iterating the next value in the list to be the head
% base case: the sum of the empty list is 0
sum_list([],0).
sum_list([Head|Tail],Sum) :-
        sum_list(Tail, Tsum),
        Sum #= Head + Tsum.
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
        Product #= Head * Tproduct.
%--------------------------------------------------------------
% cell values
% assigns values to coordinates in a cell.
% base case: empty lists, do nothing
cell_values([],[]).
cell_values([Coordhead|Coordtail], [Valuehead|Valuetail]):-
        Coordhead = Valuehead,
        cell_values(Coordtail,Valuetail).
%--------------------------------------------------------------
check_constraint(cage(id, Value, Cells), S):-
        cell_values(Cells,Value).
check_constraint(cage(add, Value,Cells), S):-
        sum_list(Cells,X),
        X#=Value.
check_constraint(cage(sub, Value, [Cell1,Cell2]), S):-
        Cell1-Cell2#=Value;Cell2-Cell1#=Value.
check_constraint(cage(mult, Value, Cells), S):-
        product_list(Cells,X),
        X#=Value.
check_constraint(cage(div, Value, [Cell1,Cell2]), S):-
       Value #= Cell1/Cell2;Value #= Cell2/Cell1.
%---------------------------------------------------------------
kenken(Puzzle):-
        Puzzle=[A,B],
        A=[A1,A2],
        B=[B1,B2],
        Co1=[A1,B1],
        Co2=[A2,B2],
        A ins 1..2,
        B ins 1..2,
        check_constraint(cage(div,1,[A1,B1]),Puzzle),
        %all_different(A),
        %all_different(B),
        %all_different(Co1),
        %all_different(Co2),
        label(A),
        label(B).

%Puzzle=[[_,_],[_,_]], Puzzle=[A,B],kenken([A,B]).
