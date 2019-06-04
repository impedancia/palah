%% -*- Mode: Prolog; coding: utf-8 -*-
:- set_prolog_flag(legacy_char_classification,on).
:- set_prolog_flag(toplevel_print_options,
     [quoted(true),numbervars(true),portrayed(true),max_depth(100)]).

:- use_module(library(lists)).

% Az előre tekintés mértéke egy felhasználótól beolvasott érték.
:- dynamic elore_tekintes/1.

% A jatek kezdtete. Megkérdezzük a felhasználótól, hogy milyen nehézségi szinten
% szeretne játszani, majd elindítjuk a játékot
start :-
    melyseg_beolvas(Melyseg),
    (retract(elore_tekintes(_)) ; true),!, assert(elore_tekintes(Melyseg)),
    init(Tabla,Jatekos), 
    jatek_kor(Tabla,Jatekos,Eredmeny).

% A nehézségi szint azt határozza meg, hogy az alfa-beta algoritmus milyen
% mélységben tekintsen előre a legjobb lépés meghatározása során.
melyseg_beolvas(Melyseg) :-
    nl, format('Kerem valasszon nehezseget (1..4): ',[]), read(M),
    (megfelelo_nehezsegi_szint(M)  -> Melyseg = M ; melyseg_beolvas(Melyseg)).

% A nehézségi szint 1-től 4-ig terjedhet.
megfelelo_nehezsegi_szint(M) :-
    member(M,[1,2,3,4]).

% Minden körben megnézzük, hogy vége van-e a játéknak. Ha igen akkor kiírjuk
% az eredményt.
jatek_kor(Tabla,Jatekos,Eredmeny) :-
    tabla_megjelenites(Tabla,Jatekos),
    vege_van_e(Tabla,Jatekos,Eredmeny), !,
    eredmenyhirdetes(Eredmeny).

% Ha nem volt vége a játéknak, akkor lépést választunk, lépünk, és a következő
% játékosnak adjuk át az irányítást.
jatek_kor(Tabla,Jatekos,Eredmeny) :-
    lepes_valasztas(Tabla,Jatekos,Lepes),
    lep(Lepes,Tabla,Tabla1),
    kovetkezo_jatekos(Jatekos,Jatekos1),!, 
    jatek_kor(Tabla1,Jatekos1,Eredmeny).

% N darab szóköz karakter kiírása.
tab(N) :-
    build(' ',N,O),
    format(O, []).

% A tab szabály kiegészítője, egy N hosszú listát készít, melynek minden eleme E.
build(E,N,L):-
    length(L,N),
    maplist(=(E),L).

% A számítógép a játékfa kiértékelését elvégezve dönt a következő lépésről.
lepes_valasztas(Tabla,computer,Lepes) :-
    elore_tekintes(Melyseg), 
    alpha_beta(Melyseg,Tabla,-40,40,Lepes,Ertek),
    nl, write(Lepes), nl.

% A játékos lépését a felhasználótól kérdezzük meg.
lepes_valasztas(Tabla,ellenfel,Lepes) :- 
    lepes_beolvas(Lepes, Tabla).

% A lépést addig olvassuk be, ameddig nem kapunk valós lehetséges értéket.
lepes_beolvas(Lepes, Tabla) :-
    nl, format('Kerem valasszon lepes(eke)t: ',[]), read(L)
    ,(cheat_e(L) -> lepes_valasztas(Tabla, computer, Lepes) ; (szabalyos_e(L, Tabla) -> Lepes = L ; lepes_beolvas(Lepes, Tabla) )).
    
% Egy lépés kiértékelése az alfa-béta algoritmus segítségével.
kiertekel([Lepes|Lepesek],Tabla,D,Alpha,Beta,Lepes1,LegjobbLepes) :-
    lep(Lepes,Tabla,Tabla1),
    alpha_beta(D,Tabla1,Alpha,Beta,LepesX,Ertek),
    Ertek1 is -Ertek,   
    levag(Lepes,Ertek1,D,Alpha,Beta,Lepesek,Tabla,Lepes1,LegjobbLepes).

% Mikor nem tudunk több lépést kiértékelni a lépés értéke az alfa parméter.
kiertekel([],Tabla,D,Alpha,Beta,Lepes,(Lepes,Alpha)).

% Mikor a mélységi korlátot elérjük az alfa-béta algoritmusban, az eddig
% kiszámolt érték lesz a lépés értéke.
alpha_beta(0,Tabla,Alpha,Beta,Lepes,Ertek) :- 
    ertek(Tabla,Ertek).

% Ha a mélységi korlátot még nem értük el, akkor az alfa-béta algoritmus
% az eddig kiértékelt játékfa mélységét, illetve a paraméterek frissítését
% elvégzi.
alpha_beta(D,Tabla,Alpha,Beta,Lepes,Ertek) :- 
    findall(M,lep(Tabla,M),Lepesek),
    Alpha1 is -Beta, 
    Beta1 is -Alpha, 
    D1 is D-1,
    kiertekel(Lepesek,Tabla,D1,Alpha1,Beta1,nil,(Lepes,Ertek)).

% Az alfa-béta algoritmus vágását megvalósító szabály, nem értékelünk
% tovább, ha az aktuális érték nagyobb mint a beta.				%
levag(Lepes,Ertek,D,Alpha,Beta,Lepesek,Tabla,Lepes1,(Lepes,Ertek)) :-
    Ertek >= Beta.

% A következő ket szabály akkor alkalmazandó, ha nem tudunk vágni,
% és a maximum kiválasztást valósítja meg.
levag(Lepes,Ertek,D,Alpha,Beta,Lepesek,Tabla,Lepes1,LegjobbLepes) :-
    Alpha < Ertek, Ertek < Beta, 
    kiertekel(Lepesek,Tabla,D,Ertek,Beta,Lepes,LegjobbLepes).

levag(Lepes,Ertek,D,Alpha,Beta,Lepesek,Tabla,Lepes1,LegjobbLepes) :-
    Ertek =< Alpha, 
    kiertekel(Lepesek,Tabla,D,Alpha,Beta,Lepes1,LegjobbLepes).

% A lépések során elképzelhető, hogy egy körön belül valaki többet is lép.
% Ez a szabály megadja a lehetséges lépéseket.
lep(Tabla,[M|Ms]) :- 
    member(M,[1,2,3,4,5,6]), 
    darab(M,Tabla,N),
    lepesek_kiterjesztese(N,M,Tabla,Ms).

% Üres lyukakkal nem lehetséges lépni.
lep(tabla([0,0,0,0,0,0],K,Ys,L),[]).

% Megadja, hogy az M-edik lyukban mennyi kő található.
darab(M,tabla(Hs,K,Ys,L),Kovek) :-
    n_edik_elem(M,Hs,Kovek), Kovek > 0.

% Az újabb lépés feltétele, hogy az aktuális lépés utolsó köve a
% kalahba essen. Ez a szabály akkor lesz sikeres, ha nincs új lépés.
lepesek_kiterjesztese(Kovek,M,Tabla,[]) :-
    Kovek =\= (7-M) mod 13, !.

% Ez a szabály akkor sikeres, ha új lépés engedélyezett, ekkor
% a lépés maradék-tagja is ellenőrzésre kerül.
lepesek_kiterjesztese(Kovek,M,Tabla,Ms) :- 
    Kovek =:= (7-M) mod 13, !, 
    kovek_kiosztasa(Kovek,M,Tabla,Tabla1),
    lep(Tabla1,Ms).

% A lépés szimulálását végző szabály.
lep([N|Ns],Tabla,VegsoTabla) :- 
    darab(N,Tabla,Kovek),
    kovek_kiosztasa(Kovek,N,Tabla,Tabla1),
    lep(Ns,Tabla1,VegsoTabla).

% A lépések végrehajtása során ha a lépés végére értünk, megcseréljük
% a két táblarészt.
lep([],Tabla1,Tabla2) :-
    csere(Tabla1,Tabla2).

% A kövek egyenkénti kosztása.
kovek_kiosztasa(Kovek,Lyuk,Tabla,VegsoTabla) :-
    sajat_kiosztas(Kovek,Lyuk,Tabla,Tabla1,Kovek1),
    masik_kiosztas(Kovek1,Tabla1,VegsoTabla).

% A kövek saját térfélen való kiosztása.
sajat_kiosztas(Kovek,N,tabla(Hs,K,Ys,L),tabla(Hs1,K1,Ys,L),Kovek1) :-
    Kovek > 7-N, !, 
    felvesz_es_kioszt(N,Kovek,Hs,Hs1),
    K1 is K+1, Kovek1 is Kovek+N-7.

% Az utolsó kő kiosztása a saját térfélen.
sajat_kiosztas(Kovek,N,tabla(Hs,K,Ys,L),Tabla,0) :-
    felvesz_es_kioszt(N,Kovek,Hs,Hs1),
    zsakmany_ellenoriz(N,Kovek,Hs1,Hs2,Ys,Ys1,Darabok),
    kalah_frissitese(Darabok,N,Kovek,K,K1),
    befejezodott_e(tabla(Hs2,K1,Ys1,L),Tabla).

% A zsákmányszerzés játékbeli szabály ellenőrzését és végrehajtását
% végző szabály.
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

% A játék végét jelző feltétel ellenőzésének része, hogy üressé vált-e
% az egyik táblarész. Ilyenkor az ellenfél táblájának összes köve az ő
% kalahjába kerül.
befejezodott_e(tabla(Hs,K,Ys,L),tabla(Hs,K,Hs,L1)) :-
    ures(Hs), !, sumlist(Ys,YsSum), L1 is L+YsSum.

% Fordított esetben is igaz a fenti szabály.
befejezodott_e(tabla(Hs,K,Ys,L),tabla(Ys,K1,Ys,L)) :-
    ures(Ys), !, sumlist(Hs,HsSum), K1 is K+HsSum.

% Ha nem ürült ki egyik oldal sem, akkor nem lép életbe az összegyűjtő
% szabály.
befejezodott_e(Tabla,Tabla) :- !.

% A kalahban lévő kövek frissítése.
kalah_frissitese(0,Kovek,N,K,K) :- Kovek < 7-N, !.
kalah_frissitese(0,Kovek,N,K,K1) :- Kovek =:= 7-N, !, K1 is K+1.
kalah_frissitese(Darabok,Kovek,N,K,K1) :- Darabok > 0, !, K1 is K+Darabok.

% Az elenfél oldalára is kiosztjuk a köveket ha saját kalahunkon túljutunk.
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

% A kövek egyenkénti kiosztásának megvalósítása.
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

% A játékállás értékelését végző függvény, a játékállás értéke a kalahokban
% lévő kövek külömbsége.
ertek(tabla(H,K,Y,L),Ertek) :- Ertek is K-L.


% Döntetlen az állás, ha a kövek fele az egyik, a másik fele a másik játékos
% kalahjában van.
vege_van_e(tabla(0,N,0,N),Jatekos,draw) :-
    darabok(K), N =:= 6*K, !.

% Egyébként a több kővel rendelkező játékos nyer.
vege_van_e(tabla(H,K,Y,L),Jatekos,Jatekos) :- 
    darabok(N), K > 6*N, !.

vege_van_e(tabla(H,K,Y,L),Jatekos,Opponent) :-
    darabok(N), L > 6*N, kovetkezo_jatekos(Jatekos,Opponent).

% Az eredmény kiírása a képernyőre.
eredmenyhirdetes(ellenfel) :- format('Ön nyert!', []).

eredmenyhirdetes(computer) :- format('Ön vesztett!', []).

eredmenyhirdetes(draw) :- format('Döntetlen.', []).

% A lista N-edik elemének kiválasztása.
n_edik_elem(N,[H|Hs],K) :-
    N > 1, !, N1 is N - 1, n_edik_elem(N1,Hs,K).

n_edik_elem(1,[H|Hs],H).

n_helyettesites(1,[X|Xs],Y,[Y|Xs]) :- !.

n_helyettesites(N,[X|Xs],Y,[X|Xs1]) :-
    N > 1, !, N1 is N-1, n_helyettesites(N1,Xs,Y,Xs1).

% A számítógép után a játékos, a játékos után a számítógép jön.
kovetkezo_jatekos(computer,ellenfel).	
kovetkezo_jatekos(ellenfel,computer).

% A kapott lépés szabályosságát ellenőrzi, figyelembe véve, hogy
% több lépés egymásutánja is szabályos-e.
szabalyos_e([N|Ns], Tabla) :- 
    lep(Tabla, [N|Ns]),
    szabalyos_e(Ns, Tabla).
szabalyos_e([],Tabla).

cheat_e([N|NS]) :-
    N = 9999.

% A két táblarész cseréje.
csere(tabla(Hs,K,Ys,L),tabla(Ys,L,Hs,K)).

% A megjelenítési logika.
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


% Üres lyukak leírása.
ures([0,0,0,0,0,0]).

nem_ures(Hs) :- Hs \== [0,0,0,0,0,0].

% Az alfa-béta algroitmus előre-tekintési paramétere.
% Ezt a program indításkor bekéri a felhasználótól.
elore_tekintes(2).

% Tábla, és kezdőjátékos meghatározása.
init(tabla([N,N,N,N,N,N],0,[N,N,N,N,N,N],0),ellenfel) :-
    darabok(N).

% Ennyi darab kő van egy lyukban.
darabok(6).
