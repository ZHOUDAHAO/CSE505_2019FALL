append([], X, X).                                   
append([X | Y], Z, [X | W]) :- append(Y, Z, W).


member(X, [X|_]).        
member(X, [_|Tail]):-member(X, Tail).

/*
list multiplication
used to count the probabilistic distribution over the possible worlds
*/
prod_list([],[],0).
prod_list([H],[1],H).
prod_list([H],[0],1-H).
prod_list([H|T], [X|Y], Product) :- 
	prod_list(T, Rest), 
	(X =:= 1 -> Product is H * Rest;
	Product is (1 - H) * Rest).

/*
count the success probability of a query
*/
prob_query([],[],0).
prob_query([H|T],[A|B],qval):-
	prod_list(H,A,Product),
	prob_query(T,B,Rest),
	qval is Product + Rest.


/*
dic stores the probability values of atoms
*/
generate_one([],[],[]):-
generate_one([PBK|R1],[PE|R2],R):-
	(dic.PBK < PE -> generate_one(R1,R2,R);
		generate_one(R1,R2,R3), R is [PBK|R3]).


list_empty([], true).
list_empty([_|_], false).


loop1(R1, R_all, R_new):-
	\+ list_empty(R_new),
	append(R_all, R_new, R),
	sort(R, R_all),
	CAND(R_new, PE, R_pru),
	findall([X,Y],(member(X,R1),member(Y,R_pru)),R_new).
	loop1(R1, R_all, R_new).


loop2(R1, R_all, R_new):-
	\+ list_empty(R_new),
	append(R_all, R_new, R),
	sort(R, R_all),
	COR(R_new, PE, R_pru),
	findall([X,Y],(member(X,R1),member(Y,R_pru)),R_new).
	loop2(R1, R_all, R_new).

/*
PILP_algorithm
*/
PILP(PBK, PE, T_all):-
	generate_one(PBK,PE,R1),
	R_new is R1,
	R_all is [],
	loop1(R1, R_all, R_new),
	T1 is R_all,
	T_new is T1,
	loop2(T1, T_all, T_new)

all_smaller([],[]).
all_smaller([X1|X],Y):-
	dic.X1 < Y,
	exist_smaller(X,Y).

	
all_greater([],[]).
all_greater([X1|X],Y):-
	dic.X1 > Y,
	exist_greater(X,Y).


exist_smaller(X,Y):-dic.X<Y.
exist_smaller([X1|X],Y):-
	dic.X1 < Y; exist_smaller(X,Y).

	
exist_greater(X,Y):-dic.X>Y.
exist_greater([X1|X],Y):-
	dic.X1 > Y;exist_greater(X,Y).


list_sum([],0).
list_sum([Item], dic.Item).
list_sum([Item1,Item2 | Tail], Total) :-
    list_sum([dic.Item1+dic.Item2|Tail], Total).


length([],0).
length([H|T],L+1) :- length(T,L).


AND_soft(X,Y):-
	list_sum(X,S),
	length(X,L),
	S-L*Y<0.
	

OR_soft(X,Y):-
	list_sum(X,S),
	length(X,L),
	S-L*Y>0.


/*
CAND Criterions
*/
CAND([],[],[]).
CAND([X|R_new], [Y|PE], R_pru):-
	(AND_soft(X,Y) -> CAND(R_new, PE, R_pru);
		CAND(R_new, PE, R), R_pru is [X|R].) 

CAND_hard([],[],[]).
CAND_hard([X|R_new], [Y|PE], R_pru):-
	(exist_smaller(X,Y) -> CAND_hard(R_new, PE, R_pru);
		CAND_hard(R_new, PE, R), R_pru is [X|R].) 

CAND_safe([],[],[]).
CAND_safe([X|R_new], [Y|PE], R_pru):-
	(all_smaller(X,Y) -> CAND_safe(R_new, PE, R_pru);
		CAND_safe(R_new, PE, R), R_pru is [X|R].) 


/*
COR Criterions
*/
COR([],[],[]).
COR([X|R_new], [Y|PE], R_pru):-
	(OR_soft(X,Y) -> COR(R_new, PE, R_pru);
		COR(R_new, PE, R), R_pru is [X|R].) 

COR_hard([],[],[]).
COR_hard([X|R_new], [Y|PE], R_pru):-
	(exist_greater(X,Y) -> COR_hard(R_new, PE, R_pru);
		COR_hard(R_new, PE, R), R_pru is [X|R].) 

	
COR_safe([],[],[]).
COR_safe([X|R_new], [Y|PE], R_pru):-
	(all_greater(X,Y) -> COR_safe(R_new, PE, R_pru);
		COR_safe(R_new, PE, R), R_pru is [X|R].) 
