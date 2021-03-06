%%% -*- Mode : Prolog -*-


%%% heap.pl --



%%% Gabriele Canesi
%%%  851637


:- dynamic heap_entry/4.
:- dynamic heap/2.


new_heap(H) :-
    heap(H, _S), !.

new_heap(H) :-
    asserta(heap(H, 0)), !.


heap_has_size(H, S) :-
    heap(H, S).



heap_empty(H) :-
    heap_has_size(H, 0).


heap_not_empty(H) :-
    heap_has_size(H, N),
    N > 0.



% Given the P and Q positions, it swaps them
swap(H, P, Q) :-
    heap_entry(H, P, KP, VP),
    heap_entry(H, Q, KQ, VQ),
    retract(heap_entry(H, P, KP, VP)),
    retract(heap_entry(H, Q, KQ, VQ)),
    asserta(heap_entry(H, P, KQ, VQ)),
    asserta(heap_entry(H, Q, KP, VP)).


sink(H, N) :-
    Left is N * 2,
    Right is N * 2 + 1,
    heap_entry(H, N, KN, _),
    heap_entry(H, Right, KR, _),
    heap_entry(H, Left, KL, _),
    KL =< KR,
    KL < KN, !,
    swap(H, N, Left),
    sink(H, Left).


sink(H, N) :-
    Left is N * 2,
    Right is N * 2 + 1,
    heap_entry(H, N, KN, _),
    heap_entry(H, Right, KR, _),
    heap_entry(H, Left, KL, _),
    KL > KR,
    KR < KN, !,
    swap(H, N, Right),
    sink(H, Right).
    

sink(H, N) :-
    heap_entry(H, N, KN, _),
    Left is N * 2,
    heap_entry(H, Left, KL, _),
    KN > KL, !,
    swap(H, N, Left),
    sink(H, Left).

sink(H, _) :- heap(H, _), !.



swim(H, K) :-
    K > 1,
    F is K div 2,
    heap_entry(H, F, KF, _),
    heap_entry(H, K, KK, _),
    KK < KF, !,
    swap(H, F, K),
    swim(H, F).

swim(H, _) :- heap(H, _), !.


delete_heap(H) :-
    heap(H, _), !,
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).

delete_heap(_) :- !.



heap_head(H, K, V) :-
    heap_entry(H, 1, K, V).



heap_insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    \+ heap_entry(H, _, _, V),
    heap_has_size(H, S),
    retract(heap(H, S)),
    S1 is S + 1,
    asserta(heap(H, S1)),
    asserta(heap_entry(H, S1, K, V)),
    swim(H, S1).



heap_extract(H, K, V) :-
    heap_has_size(H, 1), !,
    retract(heap_entry(H, 1, K, V)),
    retract(heap(H, 1)),
    asserta(heap(H, 0)).

heap_extract(H, K, V) :-
    heap_head(H, K, V),
    heap_has_size(H, S), !,
    swap(H, 1, S), 
    retract(heap_entry(H, S, _, _)),
    S1 is S - 1,
    retract(heap(H, S)),
    asserta(heap(H, S1)), !,
    sink(H, 1).


list_heap(H) :-
    heap(H, _),
    listing(heap_entry(H, _, _, _)).


modify_key(_, NewKey, NewKey, _) :- !.


% Case old key greater than new key
modify_key(H, NewKey, OldKey, V) :-
    NewKey < OldKey, !,
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    asserta(heap_entry(H, P, NewKey, V)),
    swim(H, P).


% case new key greater than old key
modify_key(H, NewKey, OldKey, V) :-
    NewKey > OldKey, !,
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    asserta(heap_entry(H, P, NewKey, V)),
    sink(H, P).



%%% end of file -- heap.pl --
