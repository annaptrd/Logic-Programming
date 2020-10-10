
makeTaskListWM([],[]).
makeTaskListWM([Task|TL],S):-
	findall(TaskL,(task(Task,_,D,_),extend(Task,D,TaskL)),S1),
	makeTaskListWM(TL,S2),
	append(S1,S2,S).


timetableWM([],[]).
timetableWM([TL|TLs],S):-	
	makeTaskListWM(TL,TaskList),
	timetableWM(TLs,S2),
	append([TaskList],S2,S).

makeTaskList([],[]).
makeTaskList([Task|TL],S):-
	findall(TaskL,(task(Task,_,D),extend(Task,D,TaskL)),S1),
	makeTaskList(TL,S2),
	append(S1,S2,S).


timetable([],[]).
timetable([TL|TLs],S):-	
	makeTaskList(TL,TaskList),
	timetable(TLs,S2),
	append([TaskList],S2,S).

get_machines(Deadline,MachAvs):-
	findall([M,N],machine(M,N),L),
	expand(L,Deadline,[],MachAvs).

expand([],_,MachAvs,MachAvs).
expand([[M,N]|L],Deadline,MachAvs1,MachAvs4):-
	expand_one(M,N,Deadline,MachAvs2),
	append(MachAvs1,MachAvs2,MachAvs3),
	expand(L,Deadline,MachAvs3,MachAvs4).


expand_one(_,0,_,[]).
expand_one(M,N,Deadline,[[M,Avs]|MachAvs]):-
	N>0,
	N1 is N-1,
	length(Avs,Deadline),
	expand_one(M,N1,Deadline,MachAvs).


mydelete(X,[[X|T]|Tail],[T|Tail]):- % epistrefei ti lista xwris to stoixeio X
	length(T,N),N>0,!.
mydelete(X,[[X|[]]|Tail],Tail).
mydelete(X,[[Head|T]|Tail],[[Head|T]|Tail1]):-
     mydelete(X,Tail,Tail1).


jobshop(S1):-
	deadline(Deadline),
	get_machines(Deadline,MachAvs),
	findall(TL,job(_,TL),Tasks),
	timetable(Tasks,TLs),%TLs = [[[t11, t11], [t12, t12, t12, t12, t12, t12]], [[t21..
	solve(MachAvs,TLs,S,R),%epistrefei oles tis pithanes lyseis gia tasks se katallila machines
	checkPriority(Tasks,R),%elegxei an isxyei i proteraiotita( px to t11 prin to t12)
	fixList(MachAvs,R,S1).%ftiaxnei to apotelesma sti zitoumeni morfi

jobshop_with_manpower(S1):-
	deadline(Deadline),
	get_machines(Deadline,MachAvs),
	findall(TL,job(_,TL),Tasks),
	timetableWM(Tasks,TLs),
	solveWM(MachAvs,TLs,S,R),
	checkPriority(Tasks,R),
	staff(W),
	checkManpower(R,W,Deadline,0),%elegxei an eparkoun oi workers gia kathe xroniki stigmi (px 0,1,..,14)
	fixList(MachAvs,R,S1).

lastElement([],0).
lastElement([X|[]],X).
lastElement([X|L],F):-
	lastElement(L,F).

checkManpower2([],_,_,[]).
checkManpower2([t(T,N1,N2)|L],Wtemp,X,[W2|W]):-%gia ti xroniki stigmi X
	X<N2,X>=N1, %an to X einai anamesa stis stigmes ekkinis k liksis tis t
	task(T,_,_,W1),
	W2 is W1+Wtemp,!,
	checkManpower2(L,W2,X,W).
checkManpower2([t(T,N1,N2)|L],W,X,W2):-
	checkManpower2(L,W,X,W2).

checkManpower([V|Vs],W,D,D):-%gia tin teleutaia xroniki stigmi
	checkManpower2([V|Vs],0,D,W1),!,
	lastElement(W1,WNeeded),
	WNeeded=<W,!.
checkManpower([V|Vs],W,D,X):-
	X<D,
	checkManpower2([V|Vs],0,X,W1),%vriskei gia kathe xroniki stigmi posoi workers xreiazontai
	lastElement(W1,WNeeded),!,
	WNeeded=<W,!,	
	X2 is X+1,
	checkManpower([V|Vs],W,D,X2),!.


fixList2([],_,[]).
fixList2([X|L],R,[S1|S]):-
	getT2(X,R,S1),!,
	fixList2(L,R,S).

getT2(_,[],_).
getT2(X,[t(T,N1,N2)|L],t(T,N1,N2)):-X=T.
getT2(X,[t(T,N1,N2)|L],S):-
	getT2(X,L,S).


fixList([],_,[]).
fixList([[M1,M2]|ML],R,[execs(M1,S)|S1]):- % ftiaxnei to teliko apotelesma stin epithymiti morfi
	set(M2,M3),
	fixList2(M3,R,S),!,
	fixList(ML,R,S1).

checkPriority([],_).
checkPriority([X|L],R):- % elegxei an isxyei i proteraiotita(t11 prin to t12)
	checkPriority2(X,R,-1),% to -1 to xrisimopoiw gia na ksexwrizw tin prwti klisi
	checkPriority(L,R).

checkPriority2([],_,_).
checkPriority2([X|L],S,-1):- 
	checkPriority3(X,S,Nnew),!,% an einai to prwto stoixeio tis listas(px to t11 ap tin [t11,t12])
	checkPriority2(L,S,Nnew).
checkPriority2([X|L],S,Nold):-
	checkPriority3(X,S,Nnew),
	task(X,_,T),
	Nnew2 is Nnew-T,%Nnew o xronos ekkinisis tou neou task
	Nnew2>=Nold,%sygkrinei ton xrono ekkinisis tou neou task me ton xrono oloklirwsis tou proigoumenou 
	checkPriority2(L,S,Nnew).

checkPriority3(_,[],_).
checkPriority3(X,[Y|L],N2):-%epistrefei ton xrono pou teleiwnei ayto to task
	getT(X,Y,N2),!.
checkPriority3(X,[Y|L],N2):-
	checkPriority3(X,L,N2).

getT(X,t(T,N1,N2),N2):-X=T.

set([],[]).
set([X|L],S):-%i gnwsti set pou exoume kanei sto mathima
	member(X,L),!,
	set(L,S).
set([X|L],[X|S]):-
	set(L,S).

solve([[M1,M2]|ML],X,S,R):-
	solve1([[M1,M2]|ML],X,X,R).

solve1(_,[],_,[]).
solve1([[M1,M2]|ML],[[T|TL]|TLs],S,[R1|R]):-%to S einai ta tasks pou prepei na valoume stis listes twn machines
	solve2([[M1,M2]|ML],T,S,S1,R1),%kalei tin solve2 gia kathe set tasks(px [t11,t12])
	solve1([[M1,M2]|ML],S1,S1,R).

solve2([],_,_,_,_).
solve2([[M1,M2]|ML],T,S,S2,R1):-
	firstElement(T,T1),
	task(T1,M,_),
	M=M1,!,
	solve3([[M1,M2]|ML],T,S,S2,R1).	%kalei tin solve3 gia kathe task(px t11)
solve2([[M1,M2]|ML],T,S,S2,R1):-
	solve2(ML,T,S,S2,R1).

solve3([],_,_,_).
solve3([[M1,M2]|ML],T,S,S2,R1):- 
	firstElement(T,T1),
	task(T1,M,_),
	M=M1,
	sublist(T,M2,R1),% to vazei sti lista kai pairnei to R(to t(t11,0,2) px)
	mydelete(T,S,S2).% diagrafei to task ayto ap ti lista twn tasks pou exoume na valoume
solve3([[M1,M2]|ML],T,S,S2,R):-
	solve3(ML,T,S,S2,R).

solveWM([[M1,M2]|ML],X,S,R):-
	solve1WM([[M1,M2]|ML],X,X,R).

solve1WM(_,[],_,[]).
solve1WM([[M1,M2]|ML],[[T|TL]|TLs],S,[R1|R]):-
	solve2WM([[M1,M2]|ML],T,S,S1,R1),
	solve1WM([[M1,M2]|ML],S1,S1,R).

solve2WM([],_,_,_,_).
solve2WM([[M1,M2]|ML],T,S,S2,R1):-
	firstElement(T,T1),
	task(T1,M,_,_),
	M=M1,!,
	solve3WM([[M1,M2]|ML],T,S,S2,R1).	
solve2WM([[M1,M2]|ML],T,S,S2,R1):-
	solve2WM(ML,T,S,S2,R1).

solve3WM([],_,_,_).
solve3WM([[M1,M2]|ML],T,S,S2,R1):- 
	firstElement(T,T1),
	task(T1,M,_,_),
	M=M1,
	sublistWM(T,M2,R1),
	mydelete(T,S,S2).
solve3WM([[M1,M2]|ML],T,S,S2,R):-
	solve3WM(ML,T,S,S2,R).

firstElement([X|Xs],X).
firstElement(X,X).

extend(_,0,[]).
extend(T,D,[T|TL]):- % opws deixtike sto mathima
	D>0,
	D1 is D-1,
	extend(T,D1,TL).

sublistWM(S,L,t(S1,N2,T1)):-%to vazei sti lista aytis tis machine kai epistrefei to katallilo t(..,..,..)
	firstElement(S,S1),
	task(S1,M,T,_),
	append(_,L2,L),
	append(S,_,L2),length(L2,N),length(L,N1),
	N2 is N1-N,
	T1 is T+N2.

sublist(S,L,t(S1,N2,T1)):-%to vazei sti lista aytis tis machine kai epistrefei to katallilo t(..,..,..)
	firstElement(S,S1),
	task(S1,M,T),
	append(_,L2,L), 
	append(S,_,L2),length(L2,N),length(L,N1),
	N2 is N1-N,
	T1 is T+N2.
