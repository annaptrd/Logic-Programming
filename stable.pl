%kapoioi endeiktikoi xronoi sta ubuntu mou:
%gia 150 zeugaria vriskei 39 lyseis se 166.76s
%gia 150 zeugaria vriskei 91 lyseis se 196.82s
%gia 100 zeygaria vriskei 56 lyseis se 56.28s
%gia 50 zeygaria vriskei 28 lyseis se 2.15s

:-lib(fd).

men([rick,jack,john,hugh,greg,nick,bill,andy,alan,dick]).

women([helen,tracy,linda,sally,wanda,maria,diana,patty,jenny,lilly]).

prefers(rick,[tracy,linda,jenny,maria,wanda,sally,diana,patty,helen,lilly]).
prefers(jack,[tracy,lilly,patty,sally,diana,linda,jenny,helen,maria,wanda]).
prefers(john,[diana,jenny,wanda,linda,patty,tracy,sally,helen,lilly,maria]).
prefers(hugh,[helen,wanda,diana,maria,sally,patty,linda,lilly,jenny,tracy]).
prefers(greg,[jenny,maria,lilly,patty,sally,linda,tracy,diana,helen,wanda]).
prefers(nick,[sally,linda,diana,maria,jenny,lilly,wanda,helen,patty,tracy]).
prefers(bill,[linda,tracy,diana,patty,lilly,sally,jenny,helen,wanda,maria]).
prefers(andy,[helen,jenny,lilly,maria,sally,patty,wanda,tracy,diana,linda]).
prefers(alan,[patty,sally,maria,jenny,linda,diana,helen,lilly,tracy,wanda]).
prefers(dick,[sally,wanda,helen,maria,lilly,diana,jenny,tracy,patty,linda]).
prefers(helen,[alan,rick,nick,bill,jack,hugh,dick,john,andy,greg]).
prefers(tracy,[andy,john,alan,hugh,bill,rick,greg,dick,jack,nick]).
prefers(linda,[dick,hugh,john,nick,andy,greg,alan,jack,bill,rick]).
prefers(sally,[nick,dick,bill,rick,greg,andy,jack,hugh,alan,john]).
prefers(wanda,[greg,bill,dick,jack,john,nick,alan,andy,rick,hugh]).
prefers(maria,[john,alan,nick,rick,greg,hugh,jack,bill,andy,dick]).
prefers(diana,[greg,hugh,andy,nick,john,bill,alan,dick,jack,rick]).
prefers(patty,[alan,bill,jack,hugh,greg,dick,rick,nick,andy,john]).
prefers(jenny,[greg,dick,jack,bill,alan,john,andy,hugh,rick,nick]).
prefers(lilly,[hugh,jack,dick,nick,andy,alan,greg,rick,bill,john]).



stable(S):-
	men(Men),
	women(Women),
	def_varsM(Men,Mv),
	def_varsW(Women,Wv),
	cons(Men,Mv,Women,Wv),
	labeling(Mv),
	solution(Men,Mv,S).

cons([],[],_,_).
cons([M|ML],[Mv|MvL],W,Wv):-%ta zeugi einai M-Mv,W-Wv
	prefers(M,Women),
	element(NWife,Women,Mv),%h thesi tis gynaikas tou M
	cons2(M,Mv,NWife,W,Wv),
	cons(ML,MvL,W,Wv).

cons2(_,_,_,[],[]).
cons2(M,Mv,NWife,[W|WL],[Wv|WvL]):-
	(M#=Wv) #<=> (Mv#=W),	
	prefers(M,Women),
	element(Nwoman,Women,W),% h thesi tis W stis protimiseis tou M
	prefers(W,Men),
	element(NHusb,Men,Wv),	%h thesi tou syzygou tis W, stis protimiseis tis 
	element(NMan,Men,M),%h thesi tou M stis protimiseis tis W
	(Nwoman#<NWife) #=> (NHusb#<NMan),	
	cons2(M,Mv,NWife,WL,WvL).

solution([],[],[]).
solution([W|WL],[M|ML],[[W-M]|S]):-
	solution(WL,ML,S).

def_varsM([],[]).
def_varsM([M|ML],[W|Mw]):-
	women(Women),
	W::Women,
	def_varsM(ML,Mw).

def_varsW([],[]).
def_varsW([W|WL],[M|Wm]):-
	men(Men),
	M::Men,	
	def_varsW(WL,Wm).

