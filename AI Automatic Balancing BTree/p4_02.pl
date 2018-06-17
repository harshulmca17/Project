go:-
	write('Enter the no. of nodes : '),
	read(R),
	balancet(R,T),
	write('Obtained balanced binary tree is : '),
	write(T).

balancet(0,nil) :- !.
balancet(N,t(N,L,R)) :- N > 0,
	N0 is N - 1,
	N1 is N0//2,
	N2 is N0 - N1,
	distrib(N1,N2,NL,NR),
	balancet(NL,L), balancet(NR,R).

distrib(N,N,N,N) :- !.
distrib(N1,N2,N1,N2).
