%%% -*- Mode : Prolog -*-


%%% graph.pl --

%%% Gabriele Canesi
%%% 851637 

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.

/**
 * new_graph(+G)
 * This predicate adds G to the knowledge base	
 */
new_graph(G) :-
    graph(G), !.

new_graph(G) :-
    asserta(graph(G)), !.

/**
 * delete_graph(G) retracts every fact which identifies G
 */
delete_graph(G) :-
    retractall(graph(G)),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)),
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)).


new_vertex(G, V) :-
    graph(G),
    vertex(G, V), !.

new_vertex(G, V) :-
    graph(G),
    asserta(vertex(G, V)), !.



/*
 * If Vs is not a variable, then the program checks if all the vertices are in
 * that list.
 */

graph_vertices(G, Vs) :-
    nonvar(Vs),
    graph(G),
    findall(vertex(G, V),(graph(G), vertex(G, V)), As),
    sort(As, SortedAs),
    sort(Vs, SortedAs).

graph_vertices(G, Vs) :-
    var(Vs),
    graph(G),
    findall(vertex(G, V),(graph(G), vertex(G, V)), Vs).



list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).


/* Self loops are not created. if a vertex does not exists, this predicate
 * creates it.
 */

new_arc(G, U, V, Weight) :-
    number(Weight),
    Weight >= 0,
    new_vertex(G, U),
    new_vertex(G, V),
    retractall(arc(G, U, V, _)),
    retractall(arc(G, V, U, _)),
    asserta(arc(G, V, U, Weight)), !.
    

new_arc(G, U, V) :-
    new_arc(G, U, V, 1).


/*
 * If Es is not a variable, then the program checks if all the requested
 * arcs are in that list.
 */

graph_arcs(G, Es) :-
    is_list(Es),
    graph(G),
    check_graph_arcs(G, Es, FixedEs),
    findall(arc(G, U, V, W), arc(G, U, V, W), As),
    length(As, N),
    length(FixedEs, N).

graph_arcs(G, Es) :-
    var(Es),
    graph(G),
    findall(arc(G, U, V, W), arc(G, V, U, W), Es).




/*
 * This predicate builds a list without duplicate (or equivalent) arcs. It is
 * used by graph_arcs. It fails if an arc is not part of G.
 */

check_graph_arcs(_, [], []) :- !.
check_graph_arcs(G, [arc(G, U, V, W) | Arcs], OutArcs) :-
    member(arc(G, U, V, W), Arcs), !,
    check_graph_arcs(G, Arcs, OutArcs).

check_graph_arcs(G, [arc(G, U, V, W) | Arcs], OutArcs) :-
    member(arc(G, V, U, W), Arcs), !,
    check_graph_arcs(G, Arcs, OutArcs).

check_graph_arcs(G, [arc(G, U, V, W) | Arcs], [arc(G, U, V, W) | OutArcs]) :-
    is_arc(arc(G, U, V, W)),
    check_graph_arcs(G, Arcs, OutArcs), !.




vertex_neighbors(G, V, Ns) :-
    nonvar(Ns),
    vertex(G, V),
    vertex_neighbors(G, V, N),
    
    % sorting the lists allows to accept every permutation
    sort(N, SortedNs),
    sort(Ns, SortedNs).

vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, U, Weight), (arc(G, U, V, Weight), U \= V), As),
    findall(arc(G, V, U, Weight), (arc(G, V, U, Weight), U \= V), Bs),
    append(As, Bs, Ns).


    
adjs(G, V, Vs) :-
    nonvar(Vs),
    vertex(G, V),
    adjs(G, V, As),

    % sorting the lists allows to accept every permutation
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

    % if FileName does not terminate with .csv, then fail
    string_concat(_, '.csv', FileName),
    
    exists_file(FileName),
    csv_read_file(FileName, Rows, [functor(arc), separator(0'\t)]),

    % reset the graph if exists and load all the read arcs
    delete_graph(G),
    new_graph(G),
    graph_from_rows(G, Rows).


/*
 * This predicate builds the arcs from the csv rows list.
 */

graph_from_rows(G, []) :- graph(G), !.
graph_from_rows(G, [arc(U, V, Weight) | Rows]) :-
    new_arc(G, U, V, Weight), !,
    graph_from_rows(G, Rows).

graph_from_rows(G, [_ | Arcs]) :-
    graph_from_rows(G, Arcs), !.

write_graph(G, FileName) :-
    write_graph(G, FileName, graph).



write_graph(G, FileName, graph) :-
    string_concat(_, '.csv', FileName),
    graph(G), !,
    %open and close the stream for creating the file.
    open(FileName, write, Out),
    close(Out),
    findall(arc(U, V, Weight), arc(G, U, V, Weight), Es),
    csv_write_file(FileName, Es, [functor(arc), separator(0'\t)]).


/** 
 * All the arcs must be part of a graph in the knowledge base.
 * If one does not exist, then the predicate fails.
 */

write_graph(G, FileName, edges) :-
    string_concat(_, '.csv', FileName),

    % check if the arcs are in the knowledge base (in any drection)
    check_edges_mode_arcs(G, Zs), !,

    % open the stream for creating the file
    open(FileName, write, Out),
    close(Out),
    csv_write_file(FileName, Zs, [functor(arc), separator(0'\t)]).



/*
 * Checks if the arcs contained in the list exists in the knowledge base.
 */

check_edges_mode_arcs([], []) :- !.
check_edges_mode_arcs([arc(G, U, V, Weight) | As], [arc(U, V, Weight) | Zs]) :-
    is_arc(arc(G, U, V, Weight)),
    check_edges_mode_arcs(As, Zs), !.

is_arc(arc(G, U, V, W)) :-
    arc(G, U, V, W), !.

is_arc(arc(G, U, V, W)) :-
    arc(G, V, U, W), !.


% end of file -- graph.pl --
