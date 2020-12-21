% -*- Mode : Prolog -*-

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.

new_graph(G) :-
    graph(G), !.

new_graph(G) :-
    asserta(graph(G)), !.



atomify(X, Y) :-
    number(X),
    atom_number(Y, X), !.
atomify(X, X) :- !.

delete_graph(G) :-
    retract(graph(G)),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)), !.

delete_graph(_) :- !.
   

new_vertex(G, V) :-
    graph(G),
    vertex(G, V), !.

new_vertex(G, V) :-
    graph(G),
    asserta(vertex(G, V)), !.


graph_vertices(G, Vs) :-
    nonvar(Vs),
    graph(G),
    findall(vertex(G, V),(graph(G), vertex(G, V)), As),
    sort(As, Vs).

graph_vertices(G, Vs) :-
    var(Vs),
    graph(G),
    findall(vertex(G, V),(graph(G), vertex(G, V)), Vs).


list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).



new_arc(_, U, U, _) :- !.
new_arc(G, U, V, W) :-
    arc(G, U, V, W), !.
new_arc(G, U, V, W) :-
    arc(G, V, U, W), !.

new_arc(G, U, V, Weight) :-
    Weight >= 0,
    vertex(G, U),
    vertex(G, V),
    retract(arc(G, U, V, _)),
    retract(arc(G, V, U, _)), !,
    asserta(arc(G, V, U, Weight)).

new_arc(G, U, V, Weight) :-
    Weight >= 0,
    vertex(G, U),
    vertex(G, V),
    retract(arc(G, V, U, _)), !,
    asserta(arc(G, V, U, Weight)).
    

new_arc(G, U, V, Weight) :-
    Weight >= 0,
    vertex(G, U),
    vertex(G, V), !,
    asserta(arc(G, U, V, Weight)).

    

new_arc(G, U, V) :-
    new_arc(G, U, V, 1).



graph_arcs(G, Es) :-
    var(Es), !,
    findall(arc(G, U, V, W), arc(G, V, U, W), As),
    findall(arc(G, U, V, W), arc(G, U, V, W), Bs),
    append(As, Bs, Es).

graph_arcs(G, Es) :-
    findall(arc(G, U, V, W), arc(G, U, V, W), As),
    sort(As, SortedAs),
    sort(Es, SortedAs).


vertex_neighbors(G, V, Ns) :-
    nonvar(Ns),
    vertex(G, V),
    vertex_neighbors(G, V, N),
    sort(N, SortedNs),
    sort(Ns, SortedNs).

vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, U, Weight), arc(G, U, V, Weight), As),
    findall(arc(G, V, U, Weight), arc(G, V, U, Weight), Bs),
    append(As, Bs, Ns).



    
adjs(G, V, Vs) :-
    nonvar(Vs),
    vertex(G, V),
    adjs(G, V, As),
    sort(As, SortedAs),
    sort(Vs, SortedAs).


adjs(G, V, Vs) :-
    vertex(G, V),
    findall(vertex(G, U), arc(G, V, U, _), As),
    findall(vertex(G, U), arc(G, U, V, _), Bs),
    append(As, Bs, Vs).

list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).
list_graph(G) :-
    list_vertices(G),
    list_arcs(G).
read_graph(G, FileName) :-
    string_concat(_, '.csv', FileName),
    exists_file(FileName),
    csv_read_file(FileName, Rows, [functor(arc), separator(0'\t)]),
    delete_graph(G),
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
    check_arcs(G, Zs), !,
    open(FileName, write, Out),
    close(Out),
    csv_write_file(FileName, Zs, [functor(arc), separator(0'\t)]).

check_arcs([], []) :- !.
check_arcs([arc(G, U, V, Weight) | As], [arc(U, V, Weight) | Zs]) :-
    arc(G, U, V, Weight),
    check_arcs(As, Zs), !.
