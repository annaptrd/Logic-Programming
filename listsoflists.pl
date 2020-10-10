cart_prod(L,CP):-
	to_reverse(L,[X|V]),d1(X,V,[],CP).

to_reverse(L,R):-
	reverse2(L,R).

d1([],_,_,[]).
d1([X|Xs],[V|Vs],Out,[CP1|CP]):-
		d2(V,Vs,[X|Out],CP1),
		d1(Xs,[V|Vs],Out,CP).

d2([],[],_,[]).
d2([X|Xs],[V|Vs],Out,CP):-
		d1([X|Xs],[V|Vs],Out,CP).
d2([X|Xs],[],Out,[[X|Out]|CP]):-
		d2(Xs,[],Out,CP).

matr_transp(L,M):-t1(L,M).

t1([],[]).
t1(_,[]).
t1(L,M):-
	t(L,Out,Rest),
	write([Out|M]),nl,
	t1(Rest,[Out|M]).

t([], [], []).
t([[X|Row] | V], [X|Xs], [Row|Rest]):- 
			t(V, Xs, Rest).


reverse2(L,R) :- reverse3(L,[],R).
reverse3([],Acc,Acc).     
reverse3([X|L],Acc,R) :-
    reverse3(L,[X|Acc],R).
