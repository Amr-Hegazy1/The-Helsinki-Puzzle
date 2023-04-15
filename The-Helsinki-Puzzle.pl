grid_build(N,M) :- grid_build(N,M,0).


grid_build(N,[],N).
grid_build(N,[HM|TM],C) :-

    N > C,
    length(HM,N),
    C1 is C + 1,
    grid_build(N,TM,C1).



grid_gen(N,M) :-

    grid_build(N,G),
    populate(N,G,M).

populate(N,[GH|GT],[MH|MT]) :-

    is_list(GH),
    populate(N,GH,MH),
    populate(N,GT,MT).

populate(_,[],[]).
populate(N,[H|T],[H|T1]) :-

    \+ is_list(H),
    num_gen(1,N,Nums),
    member(H,Nums),
    populate(N,T,T1).


num_gen(F,F,[F]).
num_gen(F,L,[F|RT]) :-

    F < L,

    F1 is F + 1,
    num_gen(F1,L,RT).


check_num_grid(G) :-

    get_max(G,Max,0),
    concat_lists(G,GC),
    list_to_set(GC,GS),
    length(G,Max2),
    Max2 >= Max,
    length(GS,Max).


concat_lists([],[]).
concat_lists([GH|GT],GC) :-

    concat_lists(GT,GC1),
    append(GH,GC1,GC).








get_max([],M,M).
get_max([H|T],Max,MaxSoFar) :-

    max_list(H,M),
    MaxSoFar < M,
    get_max(T,Max,M).

get_max([H|T],Max,MaxSoFar) :-

    max_list(H,M),
    MaxSoFar >= M,
    get_max(T,Max,MaxSoFar).



acceptable_permutation(L,R):-
    permutation(L,R).


trans(M,M1) :- 

    length(M,N),
    grid_build(N,M1),
    trans(M,M1,1).

trans([],_,_).
trans([H|T],M1,N) :-

    insert_col(H,M1,N,1),
    N1 is N + 1,
    trans(T,M1,N1).

insert_col([],_,_,_).
insert_col([H|T],[[H|_]|GT],N,N) :-

    insert_col(T,GT,N,1).



insert_col(R,[[_|T]|GT],N,C) :-

    N > C,
    C1 is C + 1,
    insert_col(R,[T|GT],N,C1).


acceptable_distribution(G) :-

    trans(G,G1),
    acceptable_distribution(G,G1).


acceptable_distribution([],[]).
acceptable_distribution([GH1|GT1],[GH2|GT2]) :-

    GH1 \= GH2,
    acceptable_distribution(GT1,GT2).






distinct_rows([]).
distinct_rows([MH|MT]) :-

    \+ member(MH,MT),
    distinct_rows(MT).

distinct_columns(M):-
    trans(M,M1),
    distinct_rows(M1).

helsinki(N,G) :-

    grid_gen(N,G),
    check_num_grid(G),
    acceptable_distribution(G),
    distinct_rows(G),
    distinct_columns(G).
