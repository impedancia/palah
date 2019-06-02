%% -*- Mode: Prolog; coding: utf-8 -*-
:- set_prolog_flag(legacy_char_classification,on).
:- set_prolog_flag(toplevel_print_options,
     [quoted(true),numbervars(true),portrayed(true),max_depth(100)]).
:- use_module(library(lists)).
:- dynamic elore_tekintes/1.

start :-
    melyseg_beolvas(Melyseg),
    (retract(elore_tekintes(_)) ; true),!, assert(elore_tekintes(Melyseg)),
	init(Tabla,Jatekos), 
	jatek_kor(Tabla,Jatekos,Eredmeny).

melyseg_beolvas(Melyseg) :-
    nl, format('Kerem valasszon nehezseget (1..4): ',[]), read(M),
    (kozott(M, 1,4)  -> Melyseg = M ; melyseg_beolvas(Melyseg)).

kozott(M,A,B) :-
	member(M,[1,2,3,4]).

jatek_kor(Tabla,Jatekos,Eredmeny) :-
	%format('play1', []),
    tabla_megjelenites(Tabla,Jatekos),
    vege_van_e(Tabla,Jatekos,Eredmeny), !, eredmenyhirdetes(Eredmeny).
jatek_kor(Tabla,Jatekos,Eredmeny) :-
	%format('play2', []),
	lepes_valasztas(Tabla,Jatekos,Lepes),
	lep(Lepes,Tabla,Tabla1),
	kovetkezo_jatekos(Jatekos,Jatekos1),!, 
    jatek_kor(Tabla1,Jatekos1,Eredmeny).

tab(N) :-
    build(' ',N,O),
    format(O, []).

build(E,N,L):-
    length(L,N),
    maplist(=(E),L).


lepes_valasztas(Tabla,computer,Lepes) :-
    elore_tekintes(Melyseg), 
	alpha_beta(Melyseg,Tabla,-40,40,Lepes,Ertek),
	nl,	write(Lepes), nl.
    lepes_valasztas(Tabla,ellenfel,Lepes) :- 
		lepes_beolvas(Lepes, Tabla).
		%nl, format('Kerem valasszon lepes(eke)t: ',[]), read(Lepes),szabalyos_e(Lepes, Tabla).

lepes_beolvas(Lepes, Tabla) :-
	nl, format('Kerem valasszon lepes(eke)t: ',[]), read(L)
	,(szabalyos_e(L, Tabla) -> Lepes = L ; lepes_beolvas(Lepes, Tabla) ).
    

kiertekel([Lepes|Lepesek],Tabla,D,Alpha,Beta,Lepes1,LegjobbLepes) :-
		%format('eval_choose1\r\n',[]),
		lep(Lepes,Tabla,Tabla1),
		alpha_beta(D,Tabla1,Alpha,Beta,LepesX,Ertek),
		%format('Ertek: ~d\r\n',[Ertek]),
        Ertek1 is -Ertek,   
        levag(Lepes,Ertek1,D,Alpha,Beta,Lepesek,Tabla,Lepes1,LegjobbLepes).

    kiertekel([],Tabla,D,Alpha,Beta,Lepes,(Lepes,Alpha)).
        %format('eval_choose2\r\n',[]).

    alpha_beta(0,Tabla,Alpha,Beta,Lepes,Ertek) :- 
		%format('alpha_beta1\r\n',[]),
		ertek(Tabla,Ertek).
    alpha_beta(D,Tabla,Alpha,Beta,Lepes,Ertek) :- 
		%format('alpha_beta2\r\n',[]),
		findall(M,lep(Tabla,M),Lepesek),
        Alpha1 is -Beta, 
		Beta1 is -Alpha, 
        D1 is D-1,
%		 %format(D1,[]),
		kiertekel(Lepesek,Tabla,D1,Alpha1,Beta1,nil,(Lepes,Ertek)).

    levag(Lepes,Ertek,D,Alpha,Beta,Lepesek,Tabla,Lepes1,(Lepes,Ertek)) :- 
		%format('levag1\r\n',[]),
		Ertek >= Beta.
    levag(Lepes,Ertek,D,Alpha,Beta,Lepesek,Tabla,Lepes1,LegjobbLepes) :-
		%format('levag2\r\n',[]),
        Alpha < Ertek, Ertek < Beta, 
		kiertekel(Lepesek,Tabla,D,Ertek,Beta,Lepes,LegjobbLepes).
    levag(Lepes,Ertek,D,Alpha,Beta,Lepesek,Tabla,Lepes1,LegjobbLepes) :-
		%format('levag3\r\n',[]),
        Ertek =< Alpha, 
		kiertekel(Lepesek,Tabla,D,Alpha,Beta,Lepes1,LegjobbLepes).

     lep(Tabla,[M|Ms]) :- 
        member(M,[1,2,3,4,5,6]), 
	darab(M,Tabla,N),
        lepesek_kiterjesztese(N,M,Tabla,Ms).
     lep(tabla([0,0,0,0,0,0],K,Ys,L),[]).

     darab(M,tabla(Hs,K,Ys,L),Kovek) :-
	n_edik_elem(M,Hs,Kovek), Kovek > 0.

     lepesek_kiterjesztese(Kovek,M,Tabla,[]) :-
	Kovek =\= (7-M) mod 13, !.
     lepesek_kiterjesztese(Kovek,M,Tabla,Ms) :- 
	Kovek =:= (7-M) mod 13, !, 
        kovek_kiosztasa(Kovek,M,Tabla,Tabla1),
	lep(Tabla1,Ms).


     lep([N|Ns],Tabla,VegsoTabla) :- 
       darab(N,Tabla,Kovek),
       kovek_kiosztasa(Kovek,N,Tabla,Tabla1),
       lep(Ns,Tabla1,VegsoTabla).
     lep([],Tabla1,Tabla2) :-
	csere(Tabla1,Tabla2).

kovek_kiosztasa(Kovek,Hole,Tabla,VegsoTabla) :-
   sajat_kiosztas(Kovek,Hole,Tabla,Tabla1,Kovek1),
   masik_kiosztas(Kovek1,Tabla1,VegsoTabla).

sajat_kiosztas(Kovek,N,tabla(Hs,K,Ys,L),tabla(Hs1,K1,Ys,L),Kovek1) :-
  Kovek > 7-N, !, 
  felvesz_es_kioszt(N,Kovek,Hs,Hs1),
  K1 is K+1, Kovek1 is Kovek+N-7.
sajat_kiosztas(Kovek,N,tabla(Hs,K,Ys,L),Tabla,0) :-
  felvesz_es_kioszt(N,Kovek,Hs,Hs1),
  zsakmany_ellenoriz(N,Kovek,Hs1,Hs2,Ys,Ys1,Darabok),
  kalah_frissitese(Darabok,N,Kovek,K,K1),
  befejezodott_e(tabla(Hs2,K1,Ys1,L),Tabla).
				       
zsakmany_ellenoriz(N,Kovek,Hs,Hs1,Ys,Ys1,Darabok) :-
  VegsoLyuk is N+Kovek,
  n_edik_elem(VegsoLyuk,Hs,1),
  EllenkezoLyuk is 7-VegsoLyuk,
  n_edik_elem(EllenkezoLyuk,Ys,Y),
  Y > 0, !,
  n_helyettesites(EllenkezoLyuk,Ys,0,Ys1),
  n_helyettesites(VegsoLyuk,Hs,0,Hs1),
  Darabok is Y+1.
zsakmany_ellenoriz(N,Kovek,Hs,Hs,Ys,Ys,0) :- !.

befejezodott_e(tabla(Hs,K,Ys,L),tabla(Hs,K,Hs,L1)) :-
  ures(Hs), !, sumlist(Ys,YsSum), L1 is L+YsSum.
befejezodott_e(tabla(Hs,K,Ys,L),tabla(Ys,K1,Ys,L)) :-
  ures(Ys), !, sumlist(Hs,HsSum), K1 is K+HsSum.
befejezodott_e(Tabla,Tabla) :- !.
    
kalah_frissitese(0,Kovek,N,K,K) :- Kovek < 7-N, !.
kalah_frissitese(0,Kovek,N,K,K1) :- Kovek =:= 7-N, !, K1 is K+1.
kalah_frissitese(Darabok,Kovek,N,K,K1) :- Darabok > 0, !, K1 is K+Darabok.

masik_kiosztas(0,Tabla,Tabla) :- !.
masik_kiosztas(Kovek,tabla(Hs,K,Ys,L),tabla(Hs,K,Ys1,L)) :-
  1 =< Kovek, Kovek =< 6, 
  nem_ures(Hs), !, 
  kiosztas(Kovek,Ys,Ys1).
masik_kiosztas(Kovek,tabla(Hs,K,Ys,L),tabla(Hs,K,Ys1,L)) :-
  Kovek > 6, !, 
  kiosztas(6,Ys,Ys1),
  Kovek1 is Kovek-6,
  kovek_kiosztasa(Kovek1,0,tabla(Hs,K,Ys1,L),Tabla).
masik_kiosztas(Kovek,tabla(Hs,K,Ys,L),tabla(Hs,K,Hs,L1)) :-
  ures(Hs), !, sumlist(Ys,YsSum), L1 is Kovek+YsSum+L.


felvesz_es_kioszt(0,N,Hs,Hs1) :-
  !, kiosztas(N,Hs,Hs1).
felvesz_es_kioszt(1,N,[H|Hs],[0|Hs1]) :-
  !, kiosztas(N,Hs,Hs1).
felvesz_es_kioszt(K,N,[H|Hs],[H|Hs1]) :- 
  K > 1, !, K1 is K-1, felvesz_es_kioszt(K1,N,Hs,Hs1).

     kiosztas(0,Hs,Hs) :- !.
     kiosztas(N,[H|Hs],[H1|Hs1]) :-
        N > 0, !, N1 is N-1, H1 is H+1, kiosztas(N1,Hs,Hs1).
     kiosztas(N,[],[]) :- !.

     ertek(tabla(H,K,Y,L),Ertek) :- Ertek is K-L.

     vege_van_e(tabla(0,N,0,N),Jatekos,draw) :-
	darabok(K), N =:= 6*K, !.
     vege_van_e(tabla(H,K,Y,L),Jatekos,Jatekos) :- 
	darabok(N), K > 6*N, !.
     vege_van_e(tabla(H,K,Y,L),Jatekos,Opponent) :-
	darabok(N), L > 6*N, kovetkezo_jatekos(Jatekos,Opponent).

     eredmenyhirdetes(ellenfel) :- writeln(['On nyert!']).
     eredmenyhirdetes(computer) :- writeln(['On vesztett!']).
     eredmenyhirdetes(draw) :- writeln(['Dontetlen.']).

	n_edik_elem(N,[H|Hs],K) :-
	    N > 1, !, N1 is N - 1, n_edik_elem(N1,Hs,K).
	n_edik_elem(1,[H|Hs],H).

n_helyettesites(1,[X|Xs],Y,[Y|Xs]) :- !.
n_helyettesites(N,[X|Xs],Y,[X|Xs1]) :- 
  N > 1, !, N1 is N-1, n_helyettesites(N1,Xs,Y,Xs1).
			       
     kovetkezo_jatekos(computer,ellenfel).	
     kovetkezo_jatekos(ellenfel,computer).

     szabalyos_e([N|Ns], Tabla) :- 
	   0 < N, N < 7,
	   darab(N,Tabla,Kovek),
	   Kovek > 0,
       szabalyos_e(Ns, Tabla).
     szabalyos_e([],Tabla).

     csere(tabla(Hs,K,Ys,L),tabla(Ys,L,Hs,K)).

tabla_megjelenites(Tabla,computer) :-
kiiras(Tabla).
     tabla_megjelenites(Tabla,ellenfel) :-
	csere(Tabla,Tabla1), kiiras(Tabla1).

     kiiras(tabla(H,K,Y,L)) :-
        reverse(H,HR), 	kovek_kiirasa(HR), kalahok_kiirasa(K,L), kovek_kiirasa(Y).

     kovek_kiirasa(H) :- 
	nl, tab(5), lyukak_kiirasa(H).

     lyukak_kiirasa([H|Hs]) :-
	kupac_kiirasa(H), lyukak_kiirasa(Hs).
     lyukak_kiirasa([]) :- nl.

	kupac_kiirasa(N) :- N < 10, write(N), tab(4).
	kupac_kiirasa(N) :- N >= 10, write(N), tab(3).

     kalahok_kiirasa(K,L) :- 
        write(K), tab(34), write(L), nl.

     ures([0,0,0,0,0,0]).

     nem_ures(Hs) :- Hs \== [0,0,0,0,0,0].


    elore_tekintes(2).
init(tabla([N,N,N,N,N,N],0,[N,N,N,N,N,N],0),ellenfel) :-
	darabok(N).

     darabok(6).
