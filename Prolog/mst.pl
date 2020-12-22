% -*- Mode: Prolog -*-

/**
 * Gabriele Canesi
 * Matricola 851637 */

:- include('heap.pl').
:- include('graph.pl').
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
:- dynamic not_visited/2.

mst_init(G, Source, Heap) :-
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)),
    delete_heap(Heap),
    new_heap(Heap),
    findall(V, (vertex(G, V), V \= Source), Vs),
    findall(vertex_key(G, V, inf), (vertex(G, V), V \= Source), Vp),
    maplist(heap_insert(Heap, inf), Vs),
    maplist(asserta(), Vp),
    heap_insert(Heap, 0, Source),
    asserta(vertex_key(G, Source, 0)).


mst_prim(G, Source) :-
    vertex(G, Source),
    mst_init(G, Source, min_queue), 
    mst_prim_rec(G, min_queue).

mst_prim_rec(_, Heap) :- heap_empty(Heap), !.
mst_prim_rec(G, Heap) :- 
    heap_not_empty(Heap),
    heap_extract(Heap, K, V),
    retract(vertex_key(G, V, _)),
    asserta(vertex_key(G, V, K)),
    vertex_neighbors(G, V, Ns),
    mst_explore_neighbors(G, Ns, Heap), !,
    mst_prim_rec(G, Heap).

mst_explore_neighbors(_, [], _) :- !.
mst_explore_neighbors(G, [arc(G, Source, N, Weight) | Ns], Heap) :-
    heap_entry(Heap, _, K, N),
    Weight < K, !,
    set_previous(G, N, Source),
    modify_key(Heap, Weight, K, N),
    mst_explore_neighbors(G, Ns, Heap).

mst_explore_neighbors(G, [_ | Arcs], Heap) :-
    mst_explore_neighbors(G, Arcs, Heap), !.

set_previous(G, N, Source) :-
    retract(vertex_previous(G, N, _)), !,
    asserta(vertex_previous(G, N, Source)).
set_previous(G, N, Source) :-
    asserta(vertex_previous(G, N, Source)), !.


mst_get(G, Source, PreorderTree) :-
    vertex(G, Source),
    retractall(not_visited(G, _)),
    findall(not_visited(G, V), vertex(G, V), Vs),
    maplist(asserta(), Vs),
    mst_get_rec(G, Source, PreorderTree),
    retractall(not_visited(G, _)).


find_and_atomify_free_neighbors(G, Source, Arcs) :-
    
    findall(arc(G, Source, AV, K), (vertex_previous(G, V, Source),
				    atomify(V, AV),
				    vertex_key(G, V, K),
				    not_visited(G, V)), As),
    
    findall(arc(G, Source, AV, K), (vertex_previous(G, Source, V),
				    atomify(V, AV),
				    vertex_key(G, Source, K),
				    not_visited(G, V)), Bs),
    append(As, Bs, Arcs), !.

mst_get_rec(G, Source, PreorderTree) :-
    find_and_atomify_free_neighbors(G, Source, FreeNeighbors),
    retract(not_visited(G, Source)), !,
    sort(3, @=<, FreeNeighbors, SortedByName),
    sort(4, =<, SortedByName, SortedNeighbors),
    mst_list_get(G, SortedNeighbors, PreorderTree).


mst_get_rec(_, _, _) :- !.

mst_list_get(_, [], []) :- !.
mst_list_get(G, [arc(G, V, U, Weight) | Arcs]
	     , [arc(G, V, NU, Weight) | PreorderTree]) :-
    numberify(U, NU),
    mst_get_rec(G, NU, PreorderSubTree),
    mst_list_get(G, Arcs, OtherTree),
    append(PreorderSubTree, OtherTree, PreorderTree).


somma_archi([], 0) :- !.
somma_archi([arc(_, _, _, W) | Archi], Somma) :-
    somma_archi(Archi, S1), !,
    Somma is S1 + W.



numberify(X, Y) :-
    atom_number(X, Y), !.

numberify(X, X) :- !.