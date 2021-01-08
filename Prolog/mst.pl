%%% -*- Mode: Prolog -*-


%%% Gabriele Canesi
%%% Matricola 851637 */

:- include('heap.pl').
:- include('graph.pl').
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
:- dynamic not_visited/2.


%% This defines the name of the used heap during the Prim's algorithm
%% execution (i preferred this instead of writing its name in the predicates,
%% hope this is not a bad practice).
mst_heap_name(mst_heap).



%% here the heap containing the graph vertices is filled with the right keys,
%% and the dynamic preticates vertex_key and vertex_previous are prepared.

mst_prim(G, Source) :-
    vertex(G, Source),
    mst_heap_name(Heap), !,
    mst_init(G, Source, Heap), 
    mst_prim_rec(G, Heap).


mst_init(G, Source, Heap) :-
    % reset the previous and vertex key data and reset the heap
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

%% this is the main part of the Prim's algorithm execution.
%% after every vertex extraction from the heap, its vertex key is set to the key
%% of the extracted entry. Then will be explored its neighbors list and will be
%% called again this function, until the heap is empty.


mst_prim_rec(_, Heap) :- heap_empty(Heap), delete_heap(Heap), !.
mst_prim_rec(_, Heap) :- heap_head(Heap, inf, _), delete_heap(Heap), !.
mst_prim_rec(G, Heap) :- 
    heap_not_empty(Heap),
    heap_extract(Heap, K, V),
    retract(vertex_key(G, V, _)),
    asserta(vertex_key(G, V, K)),
    vertex_neighbors(G, V, Ns),
    mst_explore_neighbors(G, Ns, Heap),
    mst_prim_rec(G, Heap), !.



/* this predicate updates the keys in the heap, comparing them with the costs of
   the arcs in the list.
   The arcs in input are in this form: arc(G, U, V, W), where U is the current
   source, and V is one of its adjacents vertices. If the weight W is les than
   the key of the heap entry containing the value V, then the key is set to W
   and the V's previous in G is updated to U.*/

mst_explore_neighbors(_, [], _) :- !.
mst_explore_neighbors(G, [arc(G, Source, N, Weight) | Ns], Heap) :-
    heap_entry(Heap, _, K, N),
    Weight < K, !,
    retractall(vertex_previous(G, N, _)),
    asserta(vertex_previous(G, N, Source)),
    modify_key(Heap, Weight, K, N),
    mst_explore_neighbors(G, Ns, Heap).

mst_explore_neighbors(G, [_ | Arcs], Heap) :-
    mst_explore_neighbors(G, Arcs, Heap), !.


mst_get(G, Source, PreorderTree) :-
    vertex(G, Source),
    retractall(not_visited(G, _)),
    findall(not_visited(G, V), vertex(G, V), Vs),
    maplist(asserta(), Vs),
    mst_get_rec(G, Source, PreorderTree),
    retractall(not_visited(G, _)).


free_neighbors(G, Source, Arcs) :-
    
    findall(arc(G, Source, V, K), (vertex_previous(G, V, Source),
				    vertex_key(G, V, K),
				    not_visited(G, V)), As),
    
    findall(arc(G, Source, V, K), (vertex_previous(G, Source, V),
				    vertex_key(G, Source, K),
				    not_visited(G, V)), Bs),
    append(As, Bs, Arcs), !.

mst_get_rec(G, Source, PreorderTree) :-
    free_neighbors(G, Source, FreeNeighbors),
    retract(not_visited(G, Source)), !,
    predsort(arc_name_compare, FreeNeighbors, SortedByName),
    sort(4, =<, SortedByName, SortedNeighbors),
    mst_list_get(G, SortedNeighbors, PreorderTree).


mst_get_rec(_, _, _) :- !.


mst_list_get(_, [], []) :- !.
mst_list_get(G, [arc(G, V, U, Weight) | Arcs]
	     , [arc(G, V, U, Weight) | PreorderTree]) :-
    mst_get_rec(G, U, PreorderSubTree),
    mst_list_get(G, Arcs, OtherTree),
    append(PreorderSubTree, OtherTree, PreorderTree).


/* This custom compare allows not to break the program if vertex with different
 * type names are inserted (for example numbers)
 */

arc_name_compare(<, arc(_, _, V, _), arc(_, _, U, _)) :- 
    atom_string(V, SV),
    atom_string(U, SU),
    SV @< SU, !.


arc_name_compare(>, arc(_, _, V, _), arc(_, _, U, _)) :-
    atom_string(V, SV),
    atom_string(U, SU),
    SV @>= SU, !.


%%% end of file -- mst.pl --

arcs_sum([], 0) :- !.
arcs_sum([arc(_, _, _, W) | Arcs], Sum) :-
    arcs_sum(Arcs, S1), !,
    Sum is S1 + W.
