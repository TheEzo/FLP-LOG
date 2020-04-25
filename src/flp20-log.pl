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
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		convert_rules(S,NewRules),
		print_inp(NewRules),
		writeln('---------------'),
		get_vertices(NewRules,HamV),
		%% writeln('---------------'),
		test_run(NewRules,HamV,Res),
		%% writeln(Res),
		halt.


test_run([],_,[]).
test_run([_|[]],_,[]).
test_run([R|[R2|Rs]],HamV,Final) :- 
	get_vertices([R],V), get_circle([R],[R2|Rs],[],HamV,[],Res1),
	%% writeln([R|[R2|Rs]]),
	test_run([R2|Rs],HamV,_), test_run([R|Rs],HamV,_),
	writeln(Res1),
	Final=Res.

/** najde kruznici, ktera jeste neexistuje, nebo [] 
  * Rls:   predchozi usecky, ktere tvori cast kruznice
  * Rs:    usecky ke zpracovani
  * Crcls: seznam existujicich kruznic
  * HamV:  seznam vsech dostupnych bodu pres ktere ma vest kruznice
  * Slvd:  seznam vyresenych bodu pro Rls
  * Res:   Vsechny kruznice, return */
get_circle(_,[],R,_,_,R).
get_circle(Rls,[R|Rs],Crcls,HamV,Slvd,Res) :- % pravidlo pridalo dalsi bod a je ham kruznice
	%% writeln('0-----'),
	get_vertices(Rls,V), length(V,L), join_lists(Rls, [R], NewRls),
	get_vertices(NewRls, NewVerts), is_ham(NewRls,HamV),
	join_lists(Crcls,[NewRls],NewCrcls),
	get_circle(Rls,Rs,NewCrcls,HamV,Slvd,Res).
get_circle(Rls,[R|Rs],Crcls,HamV,Slvd,Res) :- % pravidlo pridalo dalsi bod a neni ham kruznice
	%% writeln('1-----'), writeln(Rls),
	get_vertices(Rls,V), length(V,L), join_lists(Rls, [R], NewRls), check_rules(NewRls,L,Slvd),
	get_vertices(NewRls, NewVerts), get_solved(NewRls,_,NewSlvd),
	get_circle(NewRls,Rs,Crcls,HamV,NewSlvd,Res).
get_circle(Rls,[R|Rs],Crcls,HamV,Slvd,Res) :- % pravidlo nepridalo dalsi bod (pop R)
	%% writeln('2-----'), writeln(Rls),
	get_circle(Rls,Rs,Crcls,HamV,Slvd,Res).
get_circle(_,_,_,_,_,_) :- writeln('BBBiiiiiiiiiiiiiiiiiig fail').



/** zkontroluje, jestli pribyl nejaky bod kruznice
  * Rls:  usecky 
  * Len:  delka vyresenych bodu bez pridaneho 
  * Slvd: Jiz vyresene vrcholy */
check_rules(Rls,Len,Slvd) :- 
	%% writeln('check1'), writeln(Rls), writeln(Slvd),
	get_vertices(Rls,V), length(V,L), get_solved(Rls,_,SlvdNew), 
	length(SlvdNew,LS2), length(Slvd,LS1), !,
	( Len+1<L -> (true); % pribyly 2 symboly
		(Len<L, LS1<LS2) -> (true); % pribyl jeden symbol + jeden se vyresil
		(false )). % spatna usecka

/** Zjisti, zda se jedna o Ham kruznici
  * usecky
  * body ktere maji tvorit kruznici */
is_ham(Rls,HamV) :- get_solved(Rls,_,Slvd), same_lists(HamV,Slvd) -> true; fail.




/************ SHOULD BE OK *****************/

/*get_solved([['A','B'],['C','A'],['C','D']],_,X)*/
/** najde vyresene vrcholy [[A,B],[C,B]] => [B]
  * pravidla primek
  * pom: projita pravidla
  * vysledek */
get_solved([],A,B) :- A=[], B=[].
get_solved([R|Rs],A,V) :- 
	get_solved(Rs,B,C), get_vertices([R],X), join_lists(B,X,A), get_duplicated(A,V).

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















/** nacte zadany pocet radku */
read_lines2([],0).
read_lines2(Ls,N) :-
	N > 0,
	read_line(L,_),
	N1 is N-1,
	read_lines2(LLs, N1),
	Ls = [L|LLs].


/** vypise seznam radku (kazdy radek samostatne) */
write_lines2([]).
write_lines2([H|T]) :- writeln(H), write_lines2(T). %(writeln je "knihovni funkce")


/** rozdeli radek na podseznamy -- pracuje od konce radku */
%zalozit prvni (tzn. posledni) seznam:
split_line2([],[[]]) :- !.
%pridat novy seznam:
split_line2([' '|T], [[]|S1]) :- !, split_line2(T,S1).
%pridat novy seznam, uchovat oddelujici znak:
split_line2([H|T], [[],[H]|S1]) :- (H=','; H=')'; H='('), !, split_line2(T,S1).
%pridat znak do existujiciho seznamu:
split_line2([H|T], [[H|G]|S1]) :- split_line2(T,[G|S1]).


/** pro vsechny radky vstupu udela split_line2 */
% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines2([],[]).
split_lines2([L|Ls],[H|T]) :- split_lines2(Ls,T), split_line2(L,H).


/** nacte N radku vstupu, zpracuje, vypise */
start2(N) :-
		prompt(_, ''),
		read_lines2(LL, N),
		split_lines2(LL,S),
		write_lines2(S).


/** prevede retezec na seznam atomu */
% pr.: string("12.35",S). S = ['1', '2', '.', '3', '5'].
retezec([],[]).
retezec([H|T],[C|CT]) :- atom_codes(C,[H]), retezec(T,CT).


/** prevede seznam cislic na cislo */
% pr.: cislo([1,2,'.',3,5],X). X = 12.35
cislo(N,X) :- cislo(N,0,X).
cislo([],F,F).
cislo(['.'|T],F,X) :- !, cislo(T,F,X,10).
cislo([H|T],F,X) :- FT is 10*F+H, cislo(T,FT,X).
cislo([],F,F,_).
cislo([H|T],F,X,P) :- FT is F+H/P, PT is P*10, cislo(T,FT,X,PT).


/** existuje knihovni predikat number_chars(?Number, ?CharList) */
% pr.: number_chars(12.35, ['1', '2', '.', '3', '5']).
