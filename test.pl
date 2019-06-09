start :-
    get_szam(N),
    format('A be�rt sz�m: ~d',[N]),nl,
    get_szam(F),
    format('A be�rt sz�m: ~d',[F]),nl.



get_szam(N) :-
    format('Adjon meg egy sz�mot:',[]), get_code(M),skip_line,
    (type_of_character(M, digit) -> number_codes(N,[M]),  number(N) ;nl, get_szam(N))
    .
type_of_character(Ch, Type) :-
             Ch >= "0", Ch =< "9",
             !,
             Type = digit.
type_of_character(Ch, other).