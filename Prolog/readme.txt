Gabriele Canesi
Mat 851637


********************************************************************************
PROJECT: MANIPULATION OF GRAPHS AND HEAPS TO GENERATE ITS MINIMUM
SPANNING TREES
********************************************************************************

INTRODUCTION
This project contains mainly two libraries: Graphs and MinHeap.
Combining these two APIs, it is possible to find the Minimum spanning tree of
undirected graphs.

Prerequisites: SWI-prolog installed. Tested on version 8.2.1 (64 bits).

--------------------------------------------------------------------------------
GRAPHS API
--------------------------------------------------------------------------------
You can use this API for creating, manipulating and getting informations about
directed graphs.

USAGE

GRAPHS MANIPULATION
Creating a graph
new_graph(G).
this predicate adds the graph G to the knowledge base, in order to be
manipulated. It is always true.

Creating a vertex
new_vertex(G, V).
this predicate adds the vertex V to the graph G, if it exists.

Creating an arc
new_arc(G, U, V, Weight).
this predicate adds a (bidirectional) arc between the vertices U and V. If one
of those don't exist, it doesn't create any arc. This predicate is always true.

Deleting a graph
delete_graph(G).
this predicate deletes the graph G. It is always true.

QUERYING INFORMATIONS ON GRAPHS

Getting a list containing all the elemens of a graph
graph_vertices(G, Vs).
this predicate is true if G is a graph and Vs is a list containing each vertex
member of G.
graph_arcs(G, Es).
this predicate is true if G is a graph and Es is a list containing each arc
member of G.



Printing the facts which describe stored in the knowledge base

list_vertices(G).
this predicate prints all the vertex(G, _, _) type facts.
list_arcs(G).
this predicate prints all the arc(G, _, _, _) type facts.
list_graph(G).
this predicate combines list_verties(G). and list_graph(G).



Getting lists of connected objects to a specific vertex
vertex_neighbors(G, V, Ns).
this predicate is true if G is a graph, V is a vertex member of G and Ns is a
list containing all the arcs starting from V in the graph G.
adjs(G, V, Vs).
this predicate is true if G is a graph, V is a vertex member of G and Vs is a
list containing all of the vertices directly connected to V in G.

INTERACTING WITH THE FILE SYSTEM
This program allows to save and load lists of arcs, operating on csv files whose
separator is tab. Each row is a arc and its structure is
    U   V   42
where U and V are vertices, and 42 is the weight.

Writing a graph to a file
write_graph(G, FileName, Type).
this predicate writes all the arcs member of G, respecting the format
previously described. If Type unifies with graph, then G is a Graph. Else, if
Type unifies with edges, G is a list of existing arcs. This predicate is true
only if FileName ends with '.csv'.


Reading a graph to file
read_graph(G, FileName).
this predicate allows to  read a sequence of arcs from FileName and add them to
G. If G exists, before reading the file, it destroys all of its vertices and arcs.


MINIMUM SPANNING TREE
building the tree starting from a source
mst_prim(G, Source).
this predicate builds the G's minimum spanning tree by asserting two types of
facts:
vertex_previous(G, U, V): V is U's parent in the tree relative to G;
vertex_key(G, V, K): V is connected to the G's minimum spanning tree by an arc
with weight K.

Traversing the Minimum Spanning Tree in Preorder
mst_get(G, Source, PreorderTree).
this predicate is true when Source is a vertex member of G, and PreorderTree is
the list of the Minimum spanning tree's arcs traversed in Preorder. 
If a vertex has more starting arcs in the tree, they are ordered by weight, and
if they have the same weight, then they are ordered alphabetically by adjacent
vertex's name.



























In order to crete a graph, you must create the graph itself:
new_graph(g).
true.

this will add a graph to the program data.
If you want to create a vertex, just run:

?- new_graph(g).
this predicate is always true.

To create a vertex, for example the vertex a, in the graph g, you need to run
the predicate:

?- new_vertex(g, a).

this predicate is always true.

assume you have two vertices, a and b. If you want to connect them with
a weighted arc, type:

?- new_arc(g, a, b, 2).
This will create one single arc weightd 2.

If you want connect two vertices with an arc weighted 1, you can type:

?- new_arc(G, V, U).

NOTE: calling new_arc/4 on a pre-existing arc, will be replace it.

You can delete a graph and all its vertices and arcs:

?- delete_graph(g).


GETTING INFORMATIONS ABOUT GRAPHS

It is possibile to get a list of arcs or vertices from a graph. For example:
if you create thw following graph and add to it some vertices/arcs,


    ?- new_graph(g).
    true.
    ?- new_vertex(g, a).
    true.
    
    ?- new_vertex(g, b).
    true.
    
    ?- new_arc(g, a, b, 2).
    true.

running the following preticates will result in:

?- graph_vertices(g, Vs).
Vs = [vertex(g, b), vertex(g, a)].

?- graph_arcs(g, Vs).
Vs = [arc(g, a, b, 2), arc(g, b, a, 2)].

They also work in the opposite way, by specifying a list to be verified.

You can also check the list of the asserted arcs, vertices or both:

?- list_arcs(g).
:- dynamic arc/4.

arc(g, b, a, 2).

true.



?- list_vertices(d).
:- dynamic vertex/2.

vertex(g, b).
vertex(g, a).

true.




?- list_graph(d).
:- dynamic vertex/2.

vertex(g, b).
vertex(g, a).

:- dynamic arc/4.

arc(g, b, a, 2).

true.

NOTE: as you can see, only arcs in one direction are asserted in the knowledge
base. That's because in this way, the queries are more efficent and the program
requires less memory to be ran.



Given the previous graph, you can also querying the list of arcs starting from
a vertex:

?- vertex_neighbors(g, a, X).
X = [arc(g, a, b, 2)].

and, similarly, querying the list of arcs:

?- adjs(g, a, Vs).
Vs = [vertex(g, b)].



INTERACTING WITH THE FILE SYSTEM
It is also possible reading and writing graphs to the File System.
In particular, if you want to load a graph from a file, it must be a CSV with
tab as separator, and each row must have this format:

    a   b   2

each row represents a arc, where a and b are vertices, and 2 is the weight.

Usage of read_graph/2:

read_graph(G, FileName),

where FileName MUST end with .csv.

NOTE: if G is a pre-existig graph, the query will destroy it.

Writing a graph
There are two modes for writing a graph: graph and edges.

Graph mode:

?- write_graph(G, FileName, graph).
true.
In this case, G is a graph name.

Not specifying the third argument will lead to a Graph mode writing:
?- write_graph(G, FileName).
true.


Edges mode:

?- write_graph(G, FileName, edges).
true.
Now, G must be a list of existing arcs.




MINIMUM SPANNING TREE
For retrieving the minimum spanning tree from a graph, there are two main
predicates:

Assume you have an undirected graph g like this:
?- graph_vertices(g, Vs).
Vs = [vertex(g,i),vertex(g,h),vertex(g,g),vertex(g,f),vertex(g,e),vertex(g,d),
      vertex(g,c),vertex(g,b),vertex(g,a)].

?- graph_arcs(g, Es).
Es = [arc(g,f,d,14),arc(g,f,c,4),arc(g,g,i,6),arc(g,c,i,2),arc(g,h,i,7),
      arc(g,h,b,11),arc(g,h,a,8),arc(g,h,g,1),arc(g,f,g,2),arc(g,f,e,10),
      arc(g,d,e,9),arc(g,d,c,7),arc(g,b,c,8),arc(g,b,a,4),arc(g,d,f,14),
      arc(g,c,f,4),arc(g,i,g,6),arc(g,i,c,2),arc(g,i,h,7),arc(g,b,h,11),
      arc(g,a,h,8),arc(g,g,h,1),arc(g,g,f,2),arc(g,e,f,10),arc(g,e,d,9),
      arc(g,c,d,7),arc(g,c,b,8),arc(g,a,b,4)].


By querying:

?- mst_prim(g, a).
true.

The program will generate the tree by asserting vertex_previous/3 relationships
between vertices:
vertex_previous(G, U, V).
where G is the graph name,

and vertex_key/3 facts:
vertex_key/vertewhich represent the weight of a arc which
connects a certain vertex .

If you want to see how that tree visited by preorder traversal appears, then
write:

?- mst_get(g, a, X).
X = [arc(g, a, b, 4), arc(g, a, h, 8), arc(g, h, g, 1), arc(g, g, f, 2),
     arc(g, f, c, 4), arc(g, c, i, 2), arc(g, c, d, 7), arc(g, d, e, 9)].


You can also visit it starting from a different source:

?- mst_get(g, c, X).
X = [arc(g,c,i,2),arc(g,c,f,4),arc(g,f,g,2),arc(g,g,h,1),arc(g,h,a,8),
     arc(g,a,b,4),arc(g,c,d,7),arc(g,d,e,9)].

The arcs starting from a vertex are ordered by weight, and if they are the same,
they are ordered alphabetically by the name of its destination vertices.



--------------------------------------------------------------------------------
MINHEAP LIBRARY
--------------------------------------------------------------------------------

Implementing an efficient Prim's Algorithm implies the utilization of a Min
priority queue. This project also implements this part.

The implementation of this library is independent from the graphs API. So, you
can use it by inserting every type of object you want.

USAGE

In order to create a Heap, you must tell the program that H is a heap:

?- new_heap(h).
true.

This will assert the fact heap(h, 0), where 0 is the initial heap size.

A heap allows the following operations:

heap_insert(H, K, V): This predicate is true if H, K, V are non-variables, H is
a heap name, K is a numeric key, and V is a non existing value in the heap.
At the end of its execution, there will be the fact

heap_entry(H, P, K, V).
where P is the heap position of the inserted elements.

heap_head(H, K, V): true if K and V are the minimum key and the associated
value.

heap_extract(H, K, V): the same as heap_head, but it also removes it from the
heap.

heap_has_size(H, S): true if S is the heap size of H.

heap_empty(H): true if H contains no elements.
heap_not_empty(H): true if H contains at least one lement.

delete_heap(H): removes a heap from the program data.

modify_key(H, NewKey, OldKey, V): That is not a common operation in this type of
data structure, but it's useful during the prim'a algorithm execution. It
changes the entry <OldKey, Value> to <NewKey, Value>.

list_heap(H): this predicate prints out everything asserted in the program which
is relative to H.
