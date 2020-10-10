%stin ektelesi mou gia to paradeigma tis ekfwnisis pou xrisimopoieitai timeout 20, teleiwnei se 8-9 sec opote den to "xreiazetai" to timeout

%ayti i xroniki veltiwsi egine epeidi epanelava ton periorismo tou Workers#=<Staff kai gia kathe xroniki stigmi gia ola ta tasks, all kai gia kathe task (vl. grammi 51)
%ousiastika epanalambanw ton periorismo parolo pou einai "perittos"(lynetai kai xwris ayton), omws etsi ginetai o elegxos pio syxna, gia kathe task kai etsi aporriptontai ekeini ti stigmi lyseis pou tha aporriptontan argotera kai ginetai arketa pio grigoro

:- lib(ic).
:- lib(branch_and_bound).


jobshop_opt(Jobs, Staff, Schedule, Cost, Delta, Timeout):-
	getSumList(Jobs,SumList),
	mysum(SumList,Sum1),
	Sum is Sum1-1,
	makeMachinesList(M,MachList),%[[m1, 0], [m1, 1], [m2, 2], [m2, 3]]		
	findall(X,machine(_,X),L),
	mysum(L,Max),	
	makeTasks(Jobs,Tasks,Sum,MachList,Sublist,Elist),
	cons_priority(Tasks),
	cons_machines(Sublist),
	flatt(Elist,EL), %i lista me olous tous xronous telous(End) twn tasks
	flatt(Sublist,SubL),
	cons_staff(SubL,Staff,Sum,0),
	Cost #= max(EL),%o max xronos End olwn twn task, tha einai to telos tou teleutaiou task k ara o synolikos xronos
	bb_min(search(SubL, 0, input_order, indomain, complete, []),Cost,bb_options{delta:Delta,timeout:Timeout}),
	flatt(Tasks,T),
	makeSchedule(T,Schedule,MachList).%to fernw stin teliki morfi

%teliki morfi
makeSchedule(_,[],[]).
makeSchedule(Tasks,[execs(M,R)|Schedule],[[M,Num]|ML]):-
	makeSchedule2(Tasks,Num,R),	
	makeSchedule(Tasks,Schedule,ML).

makeSchedule2([],_,[]).
makeSchedule2([T,M1,N,S,E,P|TL],MNum,[t(T,S,E)|R]):-
	N==MNum,
	makeSchedule2(TL,MNum,R).
makeSchedule2([T,M1,N,S,E,P|TL],MNum,R):-
	N=\=MNum,
	makeSchedule2(TL,MNum,R).

lastElement([],0).
lastElement([X|[]],X).
lastElement([X|L],F):-
	lastElement(L,F).

cons_staff2([],_,_,[],_).
cons_staff2([M,S,E,P|L],Wtemp,X,[W2|W],Staff):-
	(X#>=S and X#<E) => (W2#=Wtemp+P), %an sympiptoun xronika, athroizw tous workers tous
	(X#<S ; X#>=E) => (W2#=Wtemp), % an oxi na menei idio
	W2#=<Staff, % me tin epanalipsi tou periorismou kai edw petyxainw arketi xroniki veltiwsi
	cons_staff2(L,W2,X,W,Staff).
cons_staff([V|Vs],W,D,X):-
	X==D,
	cons_staff2([V|Vs],0,D,W1,W),
	lastElement(W1,WNeeded),
	WNeeded#=<W. %periorismos staff
cons_staff([V|Vs],W,D,X):-
	X<D,
	cons_staff2([V|Vs],0,X,W1,W),
	lastElement(W1,WNeeded),
	WNeeded#=<W,	%periorismos staff
	X2 is X+1,
	cons_staff([V|Vs],W,D,X2).

%periorismoi machines
cons_machines([]).
cons_machines([T|TL]):-
	cons_machines2(T,TL),
	cons_machines(TL).

cons_machines2([],_).
cons_machines2([[M,S,E,P]|L],TL):-
	cons_machines3([M,S,E,P],TL),
	cons_machines2(L,TL).

cons_machines3(_,[]).
cons_machines3([M1,S1,E1,P],[T|TL]):-
	cons_machines4([M1,S1,E1,P],T),
	cons_machines3([M1,S1,E1,P],TL).	

cons_machines4(_,[]).
cons_machines4([M1,S1,E1,P1],[[M,S,E,P]|L]):-
	(M1 #= M) => (E1#=<S;S1#>=E), %an xrisimopoioun idia mixani -> na mi sympiptoun xronika
	cons_machines4([M1,S1,E1,P],L).

%ftiaxnw ti lista twn machines mazi me to monadiko auksonta arithmo tous
makeMachinesList(ML,S):-
	findall(M,machine(M,_),ML),
	makeMachinesList3(ML,S,0).

makeMachinesList2(M,0,[],_).
makeMachinesList2(M,X,[[M,Num]|MList],Num):-
	X =\= 0,
	X2 is X-1,
	Num2 is Num+1,
	makeMachinesList2(M,X2,MList,Num2).

makeMachinesList3([],[],_).
makeMachinesList3([M|Ms],ML,0):-
	machine(M,X),
	makeMachinesList2(M,X,MList,0),
	Num is X,
	makeMachinesList3(Ms,ML2,Num),
	append(MList,ML2,ML).
makeMachinesList3([M|Ms],ML,Num):-
	Num =\= 0,
	machine(M,X),
	Num2 is Num+X,
	makeMachinesList2(M,X,MList,Num),
	makeMachinesList3(Ms,ML2,Num2),
	append(MList,ML2,ML).
	
%ftiaxnw tasks tis morfis [Task,Machine,MachineNumber,Start,End,People]

makeTasks([],[],_,_,[],[]).
makeTasks([J|Js],[T|T2],Sum,ML,[Sub|Sublist],[E|EL]):-
	job(J,Tasks),
	makeTasks2(Tasks,T,Sum,ML,Sub,E),
	makeTasks(Js,T2,Sum,ML,Sublist,EL).


makeTasks2([],[],_,_,[],[]).
makeTasks2([T|Ts],[[T,Machine,MachNum,S,E,People]|T2s],Sum,ML,[[MachNum,S,E,People]|Sublist],[E|EL]):-
	task(T,Machine,Time,People),
	def_vars(S,E,Time,Sum,MachNum,Machine,ML),
	makeTasks2(Ts,T2s,Sum,ML,Sublist,EL).
	

def_vars(S, E ,Time,Sum,MachNum,Machine,ML):-
   		S #:: 0..Sum,
		E #= S+Time,	
		getMachineNums(Machine,ML,MachineNums),
		MachNum #:: MachineNums. %orismos mias sygkekrimenis mixanis(arithmou)
		

%flatt opws stis simeiwseis apo to mathima
flatt([],[]).
flatt([L1|L2],FL):-
	flatt(L1,FL1),
	flatt(L2,FL2),
	append(FL1,FL2,FL).
flatt(X,[X]):-not isList(X).

isList([]).
isList([_|L]):-isList(L).


%periorismoi proteraiotitas (t11>t12>t13...)
cons_priority([]).
cons_priority([T|TL]):-
	cons_priority2(T),
	cons_priority(TL).

cons_priority2([]).
cons_priority2([[T,M,Mnum,S,E,P],[T2,M2,Mnum2,S2,E2,P2]|TL]):-
   		E#=<S2,						 %na ksekinaei afou teleiwsei to proigoumeno
		cons_priority2([[T2,M2,Mnum2,S2,E2,P2]|TL]).
cons_priority2([[T2,M2,Mnum2,S2,E2,P2]|[]]).


getMachineNums(_,[],[]).
getMachineNums(Machine,[[Machine,Num]|ML],[Num|M]):-
	getMachineNums(Machine,ML,M).
getMachineNums(Machine,[[M,N]|ML],L):-
	Machine \= M,
	getMachineNums(Machine,ML,L).	


%--athroisma--
mysum([],1).
mysum([S|SL],Sum):-
	mysum(SL,Sum2),
	Sum is Sum2+S.

getSumList([],[]).
getSumList([J|Js],T1):-
	job(J,Tasks),
	getTimes(Tasks,Times),	
	getSumList(Js,T2),
	append(Times,T2,T1).
getTimes([],[]).
getTimes([T|Ts],[X|Xs]):-
	task(T,M,X,P),
	getTimes(Ts,Xs).
