% -*- Mode : Prolog -*-

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.


new_graph(G) :-
    graph(G), !.

new_graph(G) :-
    assert(graph(G)), !.

delete_graph(G) :-
    retractall(arc(G, _, _, _)),
    retractall(vertex(G, _)),
    retract(graph(G)).

new_vertex(G, V) :-
    graph(G),
    vertex(G, V), !.

new_vertex(G, V) :-
    graph(G),
    assert(vertex(G, V)), !.

vertices(G, Vs) :-
    findall(vertex(G, V), vertex(G, V), Vs).

list_vertices(G) :-
    listing(vertex(G, _)).

new_arc(G, U, V, _) :-
    arc(G, U, V, _), !.

new_arc(G, U, V, _) :-
    arc(G, V, U, _), !.

new_arc(G, U, V, Weight) :-
    U \= V,
    Weight > 0,
    vertex(G, U),
    vertex(G, V),
    assert(arc(G, U, V, Weight)),
    assert(arc(G, V, U, Weight)), !.

new_arc(G, U, V) :-
    new_arc(G, U, V, 1).

arcs(G, Es) :-
    findall(arc(G, U, V, Weight), arc(G, U, V, Weight), Es).

neighbors(G, V, Ns) :-
    findall(arc(G, V, U, W), arc(G, V, U, W), Ns).

adjs(G, V, Vs) :-
    findall(vertex(G, U), arc(G, V, U, _), Vs).

list_arcs(G) :-
    listing(arc(G, _, _, _)).

list_graph(G) :-
    list_vertices(G),
    list_arcs(G).

read_graph(G, FileName) :-
    string_concat(_, '.csv', FileName),
    exists_file(FileName),
    graph(G), !,
    delete_graph(G),
    csv_read_file(FileName, Rows, [functor(arc), separator(0'\t)]),
    new_graph(G),
    graph_from_rows(G, Rows).

read_graph(G, FileName) :-
    string_concat(_, '.csv', FileName),
    exists_file(FileName),
    csv_read_file(FileName, Rows, [functor(arc), separator(0'\t)]),
    new_graph(G),
    graph_from_rows(G, Rows).

graph_from_rows(G, []) :- graph(G), !.
graph_from_rows(G, [arc(U, V, Weight) | Rows]) :-
    new_vertex(G, U),
    new_vertex(G, V),
    new_arc(G, U, V, Weight),
    graph_from_rows(G, Rows), !.

write_graph(G, FileName) :-
    write_graph(G, FileName, graph).

write_graph(G, FileName, graph) :-
    string_concat(_, '.csv', FileName),
    graph(G), !,
    open(FileName, write, Out),
    close(Out),
    findall(arc(U, V, Weight), arc(G, U, V, Weight), Es),
    csv_write_file(FileName, Es, [functor(arc), separator(0'\t)]).

write_graph(G, FileName, edges) :-
    string_concat(_, '.csv', FileName),
    arcs(_, Es),
    check_arcs(Es, G, Zs), !,
    open(FileName, write, Out),
    close(Out),
    csv_write_file(FileName, Zs, [functor(arc), separator(0'\t)]).

check_arcs(_, [], []) :- !.
check_arcs(Es, [arc(G, U, V, Weight) | As], [arc(U, V, Weight) | Zs]) :-
    member(arc(G, U, V, Weight), Es), !,
    check_arcs(Es, As, Zs).
