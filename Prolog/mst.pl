%%%% -*- Mode: Prolog -*-

%%%% mst.pl
% 844526 Kolyszko Matteo
% 844529 Loddo Luca
% 845374 Arizzi Sara

% ----------------------------------------------------------
:- use_module(library(csv)).

:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
% ----------------------------------------------------------

% implementazione minheap

new_heap(H) :-
    heap(H, _S),
    !.

new_heap(H) :-
    assert(heap(H, 0)),
    !.

delete_heap(H) :-
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).

heap_has_size(H, S) :-
    heap(H, S).

heap_empty(H) :-
    heap_has_size(H, 0).

heap_not_empty(H) :-
    heap_has_size(H, S),
    S > 0.

heap_head(H, K, V) :-
    heap_not_empty(H),
    heap_entry(H, 1, K, V).

update_size(H, C) :-
    heap_has_size(H, OldS),
    NewS is OldS + C,
    retract(heap(H, OldS)),
    assert(heap(H, NewS)).

heap_insert(H, K, V) :-
    heap_empty(H),
    update_size(H, 1),
    assert(heap_entry(H, 1, K, V)),
    !.

heap_insert(H, K, V) :-
    heap_has_size(H, S),
    NewS is S + 1,
    PP is floor(NewS / 2),
    heap_entry(H, PP, K1, _),
    K >= K1,
    update_size(H, 1),
    assert(heap_entry(H, NewS, K, V)),
    !.

heap_insert(H, K, V) :-
    heap_not_empty(H),
    update_size(H, 1),
    heap_has_size(H, S),
    assert(heap_entry(H, S, K, V)),
    parental_control(H, S),
    !.

modify_key(H, _, _, _) :-
    heap_empty(H),
    !,
    fail.

modify_key(H, NewKey, OldKey, V) :-
    heap_not_empty(H),
    OldKey > NewKey,
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    parental_control(H, P),
    !.

modify_key(H, NewKey, OldKey, V) :-
    heap_not_empty(H),
    OldKey < NewKey,
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    children_control(H, P),
    !.

children_control(H, P) :-
    LP is P * 2,
    heap_has_size(H, S),
    LP > S,
    !.

children_control(H, P) :-
    LP is P * 2,
    heap_has_size(H, LP),
    heap_entry(H, P, K, _),
    heap_entry(H, LP, KL, _),
    K =< KL,
    !.

children_control(H, P) :-
    LP is P * 2,
    heap_has_size(H, LP),
    heap_entry(H, P, K, _),
    heap_entry(H, LP, KL, _),
    K > KL,
    switch(H, P, LP),
    !.

children_control(H, P) :-
    LP is P * 2,
    RP is (P * 2) + 1,
    heap_entry(H, P, K, _),
    heap_entry(H, LP, KL, _),
    heap_entry(H, RP, KR, _),
    KL =< KR,
    K =< KL,
    !.

children_control(H, P) :-
    LP is P * 2,
    RP is (P * 2) + 1,
    heap_entry(H, P, K, _),
    heap_entry(H, LP, KL, _),
    heap_entry(H, RP, KR, _),
    KL =< KR,
    K > KL,
    switch(H, P, LP),
    children_control(H, LP).

children_control(H, P) :-
    LP is P * 2,
    RP is (P * 2) + 1,
    heap_entry(H, P, K, _),
    heap_entry(H, LP, KL, _),
    heap_entry(H, RP, KR, _),
    KR < KL,
    K < KR,
    !.

children_control(H, P) :-
    LP is P * 2,
    RP is (P * 2) + 1,
    heap_entry(H, P, K, _),
    heap_entry(H, LP, KL, _),
    heap_entry(H, RP, KR, _),
    KR < KL,
    K > KR,
    switch(H, P, RP),
    children_control(H, RP).

parental_control(_, 1) :-
    !.

parental_control(H, C) :-
    heap_entry(H, C, K, _),
    PP is floor(C / 2),
    heap_entry(H, PP, K1, _),
    K > K1,
    !.

parental_control(H, C) :-
    heap_entry(H, C, _, _),
    PP is floor(C / 2),
    switch(H, PP, C),
    parental_control(H, PP),
    !.

switch(H, PP, C) :-
    retract(heap_entry(H, PP, K1, V1)),
    retract(heap_entry(H, C, K, V)),
    assert(heap_entry(H, PP, K, V)),
    assert(heap_entry(H, C, K1, V1)).

heap_extract(H, _, _) :-
    heap_empty(H),
    !,
    fail.

heap_extract(H, K, V) :-
    heap_has_size(H, 1),
    retract(heap_entry(H, 1, K, V)),
    update_size(H, -1),
    !.

heap_extract(H, K, V) :-
    heap_has_size(H, S),
    retract(heap_entry(H, 1, K, V)),
    retract(heap_entry(H, S, K1, V1)),
    assert(heap_entry(H, 1, K1, V1)),
    update_size(H, -1),
    heapify(H, 1),
    !.

heapify(H, 1) :-
    heap_has_size(H, 1),
    !.

heapify(H, S) :-
    L is S * 2,
    heap_has_size(H, L),
    heap_entry(H, S, K, _V),
    heap_entry(H, L, KL, _VL),
    KL < K,
    switch(H, S, L),
    !.

heapify(H, S) :-
    L is S * 2,
    heap_has_size(H, L),
    heap_entry(H, S, K, _V),
    heap_entry(H, L, KL, _VL),
    K =< KL,
    !.

heapify(H, S) :-
    L is S * 2,
    R is (S * 2) + 1,
    heap_entry(H, S, K, _V),
    heap_entry(H, L, KL, _VL),
    heap_entry(H, R, KR, _VR),
    KL < K,
    KR < KL,
    switch(H, S, R),
    heapify(H, R).

heapify(H, S) :-
    L is S * 2,
    R is (S * 2) + 1,
    heap_entry(H, S, K, _V),
    heap_entry(H, L, KL, _VL),
    heap_entry(H, R, KR, _VR),
    KL < K,
    KL =< KR,
    switch(H, S, L),
    heapify(H, L).

heapify(H, S) :-
    L is S * 2,
    R is (S * 2) + 1,
    heap_entry(H, S, K, _V),
    heap_entry(H, L, KL, _VL),
    heap_entry(H, R, KR, _VR),
    K =< KL,
    KR < K,
    switch(H, S, R),
    heapify(H, R).

heapify(H, S) :-
    L is S * 2,
    R is (S * 2) + 1,
    heap_entry(H, S, K, _V),
    heap_entry(H, L, KL, _VL),
    heap_entry(H, R, KR, _VR),
    KL < K,
    K =< KR,
    switch(H, S, L),
    heapify(H, L).

heapify(H, S) :-
    L is S * 2,
    R is (S * 2) + 1,
    heap_entry(H, S, K, _V),
    heap_entry(H, L, KL, _VL),
    heap_entry(H, R, KR, _VR),
    K =< KL,
    K =< KR,
    !.

list_heap(H) :-
    heap(H, _),
    listing(heap(H, _)),
    listing(heap_entry(H, _, _, _)).

% ----------------------------------------------------------

% implementazione grafi

new_graph(G) :-
    graph(G),
    !.

new_graph(G) :-
    assert(graph(G)),
    !.

delete_graph(G) :-
    graph(G),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)),
    retractall(vertex_previous(G, _, _)),
    retractall(vertex_key(G, _, _)),
    retract(graph(G)).

new_vertex(G, V) :-
    graph(G),
    vertex(G, V),
    !.

new_vertex(G, V) :-
    graph(G),
    assert(vertex(G, V)).

graph_vertices(G, Vs) :-
    graph(G),
    findall(V, vertex(G, V), Vs).

list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

new_arc(G, U, V) :-
    new_arc(G, U, V, 1),
    !.

new_arc(G, U, V, W) :-
    U \= V,
    arc(G, U, V, _),
    retract(arc(G, U, V, _)),
    assert(arc(G, U, V, W)),
    !.

new_arc(G, U, V, W) :-
    U \= V,
    arc(G, V, U, _),
    retract(arc(G, V, U, _)),
    assert(arc(G, V, U, W)),
    !.

new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    U \= V,
    Weight > 0,
    assert(arc(G, U, V, Weight)).

graph_arcs(G, Es) :-
    graph(G),
    findall(arc(G, U, V, Weight), arc(G, U, V, Weight), Es).

vertex_neighbors(G, V, Ns) :-
    graph(G),
    vertex(G, V),
    findall(arc(G, U, V, Weight), arc(G, U, V, Weight), Es),
    findall(arc(G, V, U, Weight), arc(G, V, U, Weight), Es1),
    append(Es, Es1, Ns),
    !.

adjs(G, V, Vs) :-
    graph(G),
    vertex(G, V),
    findall(vertex(G, U), arc(G, U, V, Weight), Es),
    findall(vertex(G, U), arc(G, V, U, Weight), Es1),
    append(Es, Es1, Vs).

list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

list_graph(G) :-
    graph(G),
    listing(graph(G)),
    list_vertices(G),
    list_arcs(G).

read_graph(G, _) :-
    graph(G),
    !.

read_graph(G, Filename) :-
    csv_read_file(Filename, Rows, [functor(arc), separator(0'\t)]),
    length(Rows, L),
    L > 0,
    new_graph(G),
    assertion(G, Rows),
    !.

assertion(_, []) :-
    !.

assertion(G, [arc(U, V, Weight) | Tail]) :-
    new_vertex(G, U),
    new_vertex(G, V),
    new_arc(G, U, V, Weight),
    assertion(G, Tail).

write_graph(G, Filename) :-
    write_graph(G, Filename, 'graph'),
    !.

write_graph(G, Filename, Type) :-
    Type = 'edges',
    is_list(G),
    change_format(G, G2),
    csv_write_file(Filename, G2, [functor(arc), separator(0'\t)]),
    !.

write_graph(G, Filename, Type) :-
    Type = 'graph',
    graph(G),
    graph_arcs(G, Arcslist1),
    change_format(Arcslist1, Arcslist2),
    csv_write_file(Filename, Arcslist2, [functor(arc), separator(0'\t)]),
    !.

change_format([], []) :-
    !.

change_format([arc(_, U, V, Weight) | X]
	      , [arc(U, V, Weight) | Arcslist2]) :-
    change_format(X, Arcslist2).


% ----------------------------------------------------------

% implementazione algoritmo di Prim

mst_prim(G, Source) :-
    graph(G),
    vertex(G, Source),
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)),
    graph_vertices(G, Vs),
    new_heap(G),
    assert_v(G, Source, Vs),
    extract_all_heap(G, []),
    delete_heap(G),
    !.

extract_all_heap(G, _) :-
    heap_empty(G),
    !.

extract_all_heap(G, Us) :-
    heap_not_empty(G),
    heap_head(G, UK, U),
    heap_extract(G, UK, U),
    vertex_neighbors(G, U, Ns),
    append([U], Us, Us1),
    prim(G, U, Us1, Ns),
    extract_all_heap(G, Us1).

prim(_, _, _, []) :-
    !.

prim(G, U, Us, [arc(G, U, V, _) | Ns]) :-
    member(V, Us),
    prim(G, U, Us, Ns).

prim(G, U, Us, [arc(G, V, U, _) | Ns]) :-
    member(V, Us),
    prim(G, U, Us, Ns).

prim(G, U, Us, [arc(G, U, V, W) | Ns]) :-
    vertex_key(G, V, K),
    W < K,
    update_prim(G, V, U, W),
    prim(G, U, Us, Ns).

prim(G, U, Us, [arc(G, V, U, W) | Ns]) :-
    vertex_key(G, V, K),
    W < K,
    update_prim(G, V, U, W),
    prim(G, U, Us, Ns).

prim(G, U, Us, [arc(G, U, V, W) | Ns]) :-
    vertex_key(G, V, K),
    W >= K,
    prim(G, U, Us, Ns).

prim(G, U, Us, [arc(G, V, U, W) | Ns]) :-
    vertex_key(G, V, K),
    W >= K,
    prim(G, U, Us, Ns).

update_prim(G, V, U, W) :-
    retract(vertex_key(G, V, _)),
    assert(vertex_key(G, V, W)),
    heap_entry(G, _, K, V),
    modify_key(G, W, K, V),
    retract(vertex_previous(G, V, _)),
    assert(vertex_previous(G, V, U)).

assert_v(_, _, []) :-
    !.

assert_v(G, Source, [Source | Vs]) :-
    assert(vertex_key(G, Source, 0)),
    assert(vertex_previous(G, Source, nil)),
    heap_insert(G, 0, Source),
    assert_v(G, Source, Vs).

assert_v(G, Source, [V | Vs]) :-
    Source \= V,
    assert(vertex_key(G, V, inf)),
    assert(vertex_previous(G, V, nil)),
    heap_insert(G, inf, V),
    assert_v(G, Source, Vs).

mst_get(G, Source, PreorderTree) :-
    graph(G),
    vertex(G, Source),
    new_heap(G),
    mst(G, Source),
    findall(V, heap_entry(G, _, _, V), PreorderTree),
    delete_heap(G),
    !.

mst(G, U) :-
    findall(arc(G, U, V, _), vertex_previous(G, V, U), []),
    !.

mst(G, U) :-
    findall(arc(G, U, V, _), vertex_previous(G, V, U), List),
    ass_weight(List),
    order_listArcs(List, Sorted),
    empty_list(G, Sorted),
    !.

empty_list(_, []) :-
    !.

empty_list(G, [arc(G, U, V, W) | Rest]) :-
    heap_has_size(G, S),
    heap_insert(G, S, arc(G, U, V, W)),
    mst(G, V),
    empty_list(G, Rest),
    !.

ass_weight([]) :-
    !.

ass_weight([arc(G, U, V, Weight) | Rest]) :-
    arc(G, U, V, Weight),
    ass_weight(Rest).

ass_weight([arc(G, U, V, Weight) | Rest]) :-
    arc(G, V, U, Weight),
    ass_weight(Rest).


order_listArcs(ListArcs, ListArcsOrdered) :-
    intern_sort(ListArcs, [], ListArcsOrdered).

intern_sort([], Acc, Acc).

intern_sort([H | T], Acc, ListArcsOrdered) :-
    pivoting(H, T, L1, L2),
    intern_sort(L1, Acc, ListArcsOrdered1),
    intern_sort(L2, [H | ListArcsOrdered1], ListArcsOrdered).

pivoting(_, [], [], []).

pivoting(arc(Graph, U, V, Weight), [arc(Graph, U, V1, Weight1) | T]
	 , [arc(Graph, U, V1, Weight1) | L], G) :-
    Weight1 > Weight,
    pivoting(arc(Graph, U, V, Weight), T, L, G).

pivoting(arc(Graph, U, V, Weight), [arc(Graph, U, V1, Weight1) | T], L
	 , [arc(Graph, U, V1, Weight1) | G]) :-
    Weight1 < Weight,
    pivoting(arc(Graph, U, V, Weight), T, L, G).

pivoting(arc(Graph, U, V, Weight), [arc(Graph, U, V1, Weight1) | T]
	 , [arc(Graph, U, V1, Weight1) | L], G) :-
    Weight1 = Weight,
    V1 @> V,
    pivoting(arc(Graph, U, V, Weight), T, L, G).

pivoting(arc(Graph, U, V, Weight), [arc(Graph, U, V1, Weight1) | T], L
	 , [arc(Graph, U, V1, Weight1) | G]) :-
    Weight1 = Weight,
    V1 @< V,
    pivoting(arc(Graph, U, V, Weight), T, L, G).

%%%% end of file -- mst.pl
