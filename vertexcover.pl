:- lib(ic).
:- lib(branch_and_bound).
:- set_flag(print_depth, 1000).

vertexcover(N, D, C) :-
   	create_graph(N, D, Graph),   
   	def_vars(N, Nodes),     
   	state_constrs(Nodes, Graph), 
   	Cost #= sum(Nodes), 
   	bb_min(search(Nodes, 0, input_order, indomain, complete, []),Cost, _),
   	count1s(1,Nodes,C).

def_vars(NNodes, Nodes) :-
   	length(Nodes, NNodes),
   	Nodes #:: 0..1.

state_constrs(_, []).
state_constrs(Nodes, [N1 - N2| Graph]) :-
   	n_th(N1, Nodes, Node1),
   	n_th(N2, Nodes, Node2),
   	Node1 + Node2 #> 0,    
   	state_constrs(Nodes, Graph).

count1s(_,[],[]).
count1s(N,[1|L],[N|S]):-
	N1 is N+1,
	count1s(N1,L,S).
count1s(N,[0|L],S):-
	N1 is N+1,
	count1s(N1,L,S).

n_th(1, [Node| _], Node).
n_th(N, [_| Nodes], Node) :-
	N \= 1,
	N1 is N - 1,
   	n_th(N1, Nodes, Node).
