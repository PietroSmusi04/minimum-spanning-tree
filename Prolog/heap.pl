% -*- Mode : Prolog -*-

:- dynamic heap_entry/4.
:- dynamic heap/2.

new_heap(H) :-
    heap(H, _S), !.

new_heap(H) :-
    assert(heap(H, 0)), !.

heap_size(H, S) :-
    heap(H, S).

heap_empty(H) :-
    heap_size(H, 0).

heap_not_empty(H) :-
    heap_size(H, N),
    N > 0.

swap(H, P, Q) :-
    heap_entry(H, P, KP, VP),
    heap_entry(H, Q, KQ, VQ),
    retract(heap_entry(H, P, KP, VP)),
    retract(heap_entry(H, Q, KQ, VQ)),
    assert(heap_entry(H, P, KQ, VQ)),
    assert(heap_entry(H, Q, KP, VP)).
    
heapify(H, N) :-
    heap_entry(H, N, KN, VN),
    Right is N * 2 + 1,
    heap_entry(H, Right, KR, VR),
    KN > KR,
    swap(H, N, Right),
    Left is N * 2,
    heap_entry(H, Left, KL, VL),
    KN > KL,
    swap(H, N, Left), !,
    heapify(H, Left).

heapify(H, N) :-
    heap_entry(H, N, KN, VN),
    Left is N * 2,
    heap_entry(H, Left, KL, VL),
    KN > KL,
    swap(H, N, Left), !,
    heapify(H, Left).

heapify(H, N) :-
    heap_entry(H, N, _, _), !.


build_heap(H) :-
    heap_size(H, S),
    K is S div 2,
    build_heap(H, K).

build_heap(_, 0) :- !.
build_heap(H, N) :-
    N > 0,
    heapify(H, N),
    N2 is N - 1,
    build_heap(H, N2).



delete_heap(H) :-
    heap(H, _), !,
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).

delete_heap(_) :- !.

heap_head(H, K, V) :-
    heap_entry(H, 1, K, V).

heap_insert(H, K, V) :-
    heap_size(H, S),
    retract(heap(H, S)),
    S1 is S + 1,
    assert(heap(H, S1)),
    assert(heap_entry(H, S1, K, V)),
    build_heap(H).

heap_extract(H, K, V) :-
    heap_head(H, K, V),
    retract(heap_entry(H, 1, K, V)),
    heap_decrease_positions(H),
    retract(heap(H, S)),
    S1 is S - 1,
    assert(heap(H, S1)),
    build_heap(H).

heap_decrease_positions(H) :- 
    heap_size(H, S), 
    heap_decrease_positions(H, S).

heap_decrease_positions(H, 1) :- heap(H, _), !.
heap_decrease_positions(H, P) :-
    NP is P - 1,
    heap_decrease_positions(H, NP),
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, NP, K, V)), !.


list_heap(H) :-
    heap(H, _),
    listing(heap_entry(H, _, _, _)).


modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    build_heap(H).

add_to_heap(_, []) :- !.
add_to_heap(H, [(K, V) | Es]) :-
    heap_insert(H, K, V),
    add_to_heap(H, Es).
