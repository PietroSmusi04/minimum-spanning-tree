% -*- Mode: Prolog -*-



:- include('heap.pl').
:- include('graph.pl').
:- dynamic vertex_key/3.
:- dynamic previous/3.
:- dynamic not_visited/2.

mst_init(G, Source, Heap) :-
    retractall(vertex_key(G, _, _)),
    retractall(previous(G, _, _)),
    delete_heap(Heap),
    new_heap(Heap),
    findall(V, (vertex(G, V), V \= Source), Vs),
    maplist(heap_insert(Heap, inf), Vs),
    heap_insert(Heap, 0, Source).


mst_prim(G, Source) :-
    vertex(G, Source),
    mst_init(G, Source, min_queue),
    mst_prim_tail(G, min_queue).

mst_prim_tail(_, Heap) :- heap_empty(Heap), !.
mst_prim_tail(G, Heap) :-
    heap_not_empty(Heap),
    heap_extract(Heap, K, V),
    assert(vertex_key(G, V, K)),
    findall(arc(G, V, VH, W), (arc(G, V, VH, W),
			       heap_entry(Heap, _, _, VH)), Ns),
    find_previous(Heap, G, V),
    set_costs(G, Ns, Heap), !,
    mst_prim_tail(G, Heap).


set_costs(_, [], _) :- !.
set_costs(G, [arc(G, V, N, Weight) | Ns], Heap) :-
    heap_entry(Heap, _, inf, N), !,
    modify_key(Heap, Weight, inf, N),
    set_costs(G, Ns, Heap).

set_costs(G, [arc(G, V, N, Weight) | Ns], Heap) :-
    heap_entry(Heap, _, K, N),
    Weight < K, !,
    modify_key(Heap, Weight, K, N),
    set_costs(G, Ns, Heap).

set_costs(G, [A | Arcs], Heap) :-
    set_costs(G, Arcs, Heap), !.



find_previous(Heap, G, V) :-
    neighbors(G, V, Ns),
    sublist_non_heap(Heap, Ns, []), !.

find_previous(Heap, G, V) :-
    neighbors(G, V, Ns),
    append(As, Bs, Ns),
    sublist_non_heap(Heap, Ns, NonHeap),
    closest_vertex(G, NonHeap, ClosestVertex),
    assert(previous(G, V, ClosestVertex)), !.



mst_get(G, Source, PreorderTree) :-
    vertex(G, Source),
    findall(not_visited(G, V), vertex(G, V), Vs),
    maplist(assert(), Vs),
    mst_get_tail(G, Source, PreorderTree),
    retractall(not_visited(G, _)).

mst_get_tail(G, Source, []) :-
    findall(arc(G, Source, U, W), (previous(G, U, Source), not_visited(G, U)), As),
    findall(arc(G, Source, U, W), (previous(G, Source, U), not_visited(G, U)), Bs),
    append(As, Bs, Ks),
    length(Ks, 0), !.

mst_get_tail(G, Source, PreorderTree) :-
    findall(arc(G, Source, U, W), (arc(G, Source, U, W), previous(G, U, Source), not_visited(G, U)), As),
    findall(arc(G, Source, U, W), (arc(G, Source, U, W), previous(G, Source, U), not_visited(G, U)), Bs),
    append(As, Bs, Ks),
    retract(not_visited(G, Source)),
    sort_arcs(Ks, SortedKs),
    mst_list_get(G, SortedKs, PreorderTree), !.


mst_list_get(_, [], []) :- !.
mst_list_get(G, [arc(G, V, U, Weight) | Arcs]
	     , [arc(G, V, U, Weight) | PreorderTree]) :-
    mst_get_tail(G, U, PreorderSubTree), !,
    mst_list_get(G, Arcs, OtherTree),
    append(PreorderSubTree, OtherTree, PreorderTree).

mst_list_get(G, [arc(G, V, U, Weight) | Arcs]
	     ,[arc(G, V, U, Weight) | PreorderTree]) :-
    mst_get_tail(G, U, PreorderSubTree), !,
    mst_list_get(G, Arcs, OtherTree),
    append(PreorderSubTree, OtherTree, PreorderTree).

closest_vertex(G, [arc(G, V, U, Weight)], U) :- !.
closest_vertex(G, [arc(G, V, U, Weight) | Arcs], T) :-
    closest_vertex(G, Arcs, T),
    arc(G, V, T, W2),
    W2 =< Weight, !.
closest_vertex(G, [arc(G, V, U, Weight) | Arcs], U) :-
    closest_vertex(G, Arcs, T),
    arc(G, V, T, W2),
    W2 > Weight, !.


sublist_non_heap(_, [], []) :- !.
sublist_non_heap(Heap, [arc(G, V, U, Weight) | Vs], Zs) :-
    heap_entry(Heap, _, _, U), !,
    sublist_non_heap(Heap, Vs, Zs).

sublist_non_heap(Heap, [arc(G, V, U, Weight) | Vs]		
		 , [arc(G, V, U, Weight) | Zs]) :-
    sublist_non_heap(Heap, Vs, Zs), !.

sort_arcs([X], [X]) :- !.
sort_arcs(Arcs, SortedArcs) :-
    length(Arcs, L),
    M is L div 2,
    sublist(Arcs, M, Left, Right),
    sort_arcs(Left, SortedLeft),
    sort_arcs(Right, SortedRight),
    merge(SortedLeft, SortedRight, SortedArcs), !.

sublist(Xs, 0, [], Xs) :- !.
sublist([X | Xs], Cont, [X | Ls], Rs) :-
    Cont1 is Cont - 1,
    sublist(Xs, Cont1, Ls, Rs), !.

merge(Left, [], Left) :- !.
merge([], Right, Right) :- !.
merge([X | Xs], [Y | Ys], [X | Zs]) :-
    first_arc(X, Y, X), !,
    merge(Xs, [Y | Ys], Zs).

merge([X | Xs], [Y | Ys], [Y | Zs]) :-
    first_arc(X, Y, Y), !,
    merge([X | Xs], Ys, Zs).

first_arc(arc(G, V, U, W1), arc(G, _, _, W2), arc(G, V, U, W1)) :- W1 < W2, !.
first_arc(arc(G, _, _, W1), arc(G, V, Y, W2), arc(G, V, Y, W2)) :- W1 > W2, !.
first_arc(arc(G, V, U, W1), arc(G, V, Y, W1), arc(G, V, U, W1)) :- U @< Y, !.
first_arc(arc(G, V, U, W1), arc(G, V, Y, W1), arc(G, V, Y, W2)) :- U @> Y, !.

z :-
    delete_graph(X),
    delete_heap(H),
    retractall(previous(G, B, J)),
    retractall(vertex_key(P, U, Q)),
    retractall(not_visited(JJ, DD, AA)).
