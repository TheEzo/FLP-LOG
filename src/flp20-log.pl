/*******************************
  * FLP 2020 - logicky projekt *
  * Autori: Martin Hyrs        *
  * 	    Tomas Willaschek   *
  *         (xwilla00) 		   *
  ******************************/


/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
% aby to fungovalo i s retezcem na miste seznamu
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).
% G je prvni seznam ze seznamu seznamu G|S1
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). 

/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

start :-
	get_time(T1),
	prompt(_, ''),
	read_lines(LL),
	split_lines(LL,S),
	convert_rules(S,TmpRules),
	%% print_inp(TmpRules),
	remove_switched_input(TmpRules,NewRules),
	%% writeln('---------------'),
	get_vertices(NewRules,HamV),
	run(NewRules,HamV,Res),
	%% writeln('---------------'),
	remove_duplicated_res(Res,Final),
	write_output(Final),
	get_time(T2),
		DeltaT is T2- T1,
		%% write('time: '), write(DeltaT), write('  ms.\n'),
	halt.

/* Spusti get_circle s variacemi vstupu */
run([],_,[]).
run([_|[]],_,[]).
run([R|[R2|Rs]],HamV,Final) :- 
	Rule=[R|[R2|Rs]], length(HamV,L1), length(Rule,L2), L1=<L2,
	%% format('input: ~w~n', [[R|[R2|Rs]]]),
	get_circle([R],[R2|Rs],[],HamV,[],Res1),
	run([R2|Rs],HamV,Res2), join_lists(Res1,Res2,Res4),
	run([R|Rs],HamV,Res3), join_lists(Res3,Res4,Final).
run(_,_,_).




/** najde kruznici nebo [] 
  * Rls:   predchozi usecky, ktere tvori cast kruznice
  * Rs:    usecky ke zpracovani
  * Crcls: seznam existujicich kruznic
  * HamV:  seznam vsech dostupnych bodu pres ktere ma vest kruznice
  * Slvd:  seznam vyresenych bodu pro Rls
  * Res:   Vsechny kruznice, return */
get_circle(_,[],R,_,_,R).
get_circle(Rls,[R|Rs],Crcls,HamV,Slvd,Res) :- % pravidlo pridalo dalsi bod a je ham kruznice
	%% writeln('0-----'),
	join_lists(Rls, [R], NewRls), is_ham(NewRls,HamV), !, 
	join_lists(Crcls,[NewRls],NewCrcls), 
	%% format('DONE ~w----------------------~n', [NewRls]),
	get_circle(Rls,Rs,NewCrcls,HamV,Slvd,Res).
get_circle(Rls,[R|Rs],Crcls,HamV,Slvd,Res) :- % pravidlo pridalo dalsi bod a neni ham kruznice
	%% writeln('1-----'),
	get_vertices(Rls,V), length(V,L), join_lists(Rls, [R], NewRls), 
	%% format('NewRls: ~w slvd: ~w~n', [NewRls, Slvd]), 
	check_rules(NewRls,L,Slvd), !, 
	get_solved(NewRls,_,NewSlvd), get_circle(NewRls,Rs,Crcls,HamV,NewSlvd,Res1),
	%% format('Next: ~w rest: ~w~n', [Rls, Rs]), 
	get_circle(Rls,Rs,Crcls,HamV,Slvd,Res2), join_lists(Res1,Res2,Res).
get_circle(Rls,[_|Rs],Crcls,HamV,Slvd,Res) :- % pravidlo nepridalo dalsi bod (pop R)
	%% writeln('2-----'),
	get_circle(Rls,Rs,Crcls,HamV,Slvd,Res).
get_circle(_,_,_,_,_,_). %% :- writeln('BBBiiiiiiiiiiiiiiiiiig fail').


/** zkontroluje, jestli pribyl nejaky bod kruznice
  * Rls:  usecky 
  * Len:  delka vyresenych bodu bez pridaneho 
  * Slvd: Jiz vyresene vrcholy */
check_rules(Rls,Len,Slvd) :- 
	get_vertices(Rls,V), length(V,L), get_solved(Rls,_,SlvdNew), 
	length(SlvdNew,LS2), length(Slvd,LS1), 
	%% format('NewSlvd ~w~n', [SlvdNew]), writeln(Len+1<L), writeln(LS1<LS2),
	!, ((Len<L, LS1<LS2) -> (true); % pribyl jeden symbol + jeden se vyresil
		(Len+1<L -> (true); % pribyly 2 symboly
		  (LS1+1<LS2 -> (true); % vyresily se 2 symboly
		    ((false))))). % spatna usecka

/** Zjisti, zda se jedna o Ham kruznici
  * usecky
  * body ktere maji tvorit kruznici */
is_ham(Rls,HamV) :- get_solved(Rls,_,Slvd), !, same_lists(HamV,Slvd) -> true; false.

/*get_solved([['A','B'],['C','A'],['C','D']],_,X)*/
/** najde vyresene vrcholy [[A,B],[C,B]] => [B]
  * pravidla primek
  * pom: projita pravidla
  * vysledek */
get_solved([],A,B) :- A=[], B=[].
get_solved([R|Rs],A,V) :- 
	get_solved(Rs,B,_), get_vertices([R],X), join_lists(B,X,A), get_duplicated(A,V).

/* najde prvky, ktere se v seznamu vyskytuji vicekrat */
get_duplicated([],[]).
get_duplicated([X|Xs],R) :- 
	member(X,Xs), delete_all_X(X,Xs,A), get_duplicated(A,B), R=[X|B].
get_duplicated([_|Xs],R) :- get_duplicated(Xs,R).

/* smaze vsechny x z listu */
delete_all_X(X,L,V) :- member(X,L), delete(L,X,A), delete_all_X(X,A,V).
delete_all_X(_,L,L).

/* odstrani vsechny prvky A z B a ulozi do C */
remove_all([],A,A).
remove_all([A|As],B,C) :- delete(B,A,D), remove_all(As,D,C).

%% get_resolved_vert

/* upravi pravidla z [[A],[B]] na [A,B] */
convert_rules([],[]).
convert_rules([R|Rs],X) :- new_rule(R,N), convert_rules(Rs,Y), X=[N|Y].

/* upravi jedno pravidlo z [[A],[B]] na [A,B] */
new_rule([],[]).
new_rule([[V|_]|Vs],R) :- new_rule(Vs,X), R=[V|X].

/* smaze duplicity */
remove_dups([],[]).
remove_dups([X|Xs],R) :- member(X,Xs), remove_dups(Xs,R).
remove_dups([X|Xs],R) :- remove_dups(Xs,Y), R=[X|Y].

/* slouci listy A a B do C */
join_lists([],[],C) :- C=[].
join_lists([],[B|Bs],C) :- join_lists([],Bs,D), C=[B|D].
join_lists([A|As],[],C) :- join_lists(As,[],D), C=[A|D].
join_lists([A|As],B,C) :- join_lists(As,B,D), C=[A|D].

/* najde vsechny vrcholy z primek na vstupu */
get_vertices([],[]).
get_vertices([L|Ls],R) :- 
	get_line_vertices(L,X), get_vertices(Ls,Y), join_lists(X,Y,A), remove_dups(A,R).

/* vraci body primky */
get_line_vertices([],A) :- A=[].
get_line_vertices([C|Cs],R) :- get_line_vertices(Cs,X), R=[C|X].

/* debugovaci vypis vstupu */
print_inp([]).
print_inp([A|As]) :- writeln(A), print_inp(As).

/* zkontroluje, jestli prvni list je obsazen ve druhem 
 * Spousti se s hledanymi vrcholy jako 1. param. a nalezenymi vrcholy jako 2. param. */
same_lists([],_) :- true.
same_lists([A|As],B) :- member(A,B), same_lists(As,B).
same_lists(_,_) :- false.

/* Vypis na vystup v korektnim formatu */
write_output([]).
write_output([R|Rs]) :- write_line(R), writeln(''), write_output(Rs).

write_line([]).
write_line([R|[]]) :- write_item(R).
write_line([R|Rs]) :- write_item(R), write(' '), write_line(Rs).

write_item([]).
write_item([R|[]]) :- write(R).
write_item([R|Rs]) :- write(R), write('-'), write_item(Rs).

delete_last([],[]).
delete_last([_|[]],[]).
delete_last([X|Xs],Y) :- delete_last(Xs,Z), Y=[X|Z].

/* odstrani jedno z [C,D],[D,C] pokud existuje */
remove_switched_input([],[]).
remove_switched_input([X|Xs],Res) :- member_lists(X,Xs), remove_switched_input(Xs,Res).
remove_switched_input([X|Xs],Res) :- reverse_list(X,[],Y), member_lists(Y,Xs), remove_switched_input(Xs,Res).
remove_switched_input([X|Xs],Res) :- remove_switched_input(Xs,Y), Res=[X|Y].

/* otoci list naopak */
reverse_list([],X,X).
reverse_list([X|Xs],Y,Z) :- reverse_list(Xs,[X|Y],Z). 

/* smaze duplikovane vysledky */
remove_duplicated_res([],[]).
remove_duplicated_res([R|Rs],V) :- member_lists(R,Rs), remove_duplicated_res(Rs,V).
remove_duplicated_res([R|Rs],V) :- remove_duplicated_res(Rs,X), V=[R|X].

/* funkce member, ale pro listy */
member_lists(_,[]) :- false.
member_lists(R,[X|_]) :- same_lists(R,X), same_lists(X,R), true.
member_lists(R,[_|Xs]) :- member_lists(R,Xs).
