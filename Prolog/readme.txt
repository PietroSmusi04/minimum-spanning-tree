Gabriele Canesi
Mat 851637


********************************************************************************
PROJECT: MANIPULATION OF GRAPHS AND HEAPS TO GENERATE ITS MINIMUM
SPANNING TREES
********************************************************************************

Summary
1. INTRODUCTION
2. GRAPHS API
    2.1 USAGE
        2.1.1 GRAPHS MANIPULATION
        2.1.2 QUERYING GRAPHS INFORMATIONS
        2.1.3 INTERACTING WITH THE FILE SYSTEM
        2.1.4 MINIMUM SPANNING TREE
    2.2 EXAMPLES
        2.2.1 GRAPHS MANIPULATION
        2.2.2 QUERYING INFORMATIONS
        2.2.3 INTERACTING WITH THE FILE SYSTEM
        2.2.4 FINDING THE MINIMUM SPANNING TREE

3. HEAPS API
    3.1 USAGE
        3.1.1 HEAPS MANIPULATION
        3.1.2 QUERYING HEAPS INFORMATIONS
    3.2 EXAMPLES


1. INTRODUCTION
This project contains mainly two libraries: Graphs and MinHeap.
Combining these two APIs, it is possible to find the Minimum spanning tree of
undirected graphs.

Prerequisites: SWI-prolog installed. Tested on version 8.2.1 (64 bits).

--------------------------------------------------------------------------------
2. GRAPHS API
--------------------------------------------------------------------------------
You can use this API for creating, manipulating and getting informations about
directed graphs.

2.1 USAGE

2.1.1 GRAPHS MANIPULATION

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


2.1.2 QUERYING INFORMATIONS ON GRAPHS

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

2.1.3 INTERACTING WITH THE FILE SYSTEM
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
G. If G exists, before reading the file, it destroys all of its vertices and
arcs.





2.1.4 FINDING THE MINIMUM SPANNING TREE

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


2.2 EXAMPLES

2.2.1 GRAPHS MANIPULATION
Suppose you want to create a graph having these (undirected) arc and vertices:

a -> b weight 4
a -> h weight 8
b -> c weight 8
c -> d weight 7
d -> e weight 9
e -> f weight 10
d -> f weight 14
f -> g weight 2
g -> h weight 1
b -> h weight 11
g -> i weight 6
h -> i weight 7
i -> c weight 2
c -> f weight 4

First create a graph:

?- new_graph(g).
true.

Now, let's add the needed vertices:

?- new_vertex(g, a).
true.
?- new_vertex(g, b).
true.
?- new_vertex(g, c).
true.
?- new_vertex(g, d).
true.
?- new_vertex(g, e).
true.
?- new_vertex(g, f).
true.
?- new_vertex(g, g).
true.
?- new_vertex(g, h).
true.
?- new_vertex(g, i).
true.


Now you can connect the vertices:

?- new_arc(g, a, b, 4).
true.
?- new_arc(g, a, h, 8).
true.
?- new_arc(g, b, c, 8).
true.
?- new_arc(g, b, h, 11).
true.
?- new_arc(g, h, g). //(automatically sets its weight to 1).
true.
?- new_arc(g, g, f, 2).
true.
?- new_arc(g, f, c, 4).
true.
?- new_arc(g, c, i, 2).
true.
?- new_arc(g, i, h, 7).
true.
?- new_arc(g, i, g, 6).
true.
?- new_arc(g, c, d, 7).
true.
?- new_arc(g, d, e, 9).
true.
?- new_arc(g, d, f, 14).
true.
?- new_arc(g, e, f, 10).
true.


If you don't need g anymore:
delete_graph(g).
true.

2.2.2 GETTING INFORMATIONS

By assuming that there is the previously created graph in the knowledge base:
list_verties(g).
:- dynamic vertex/2.

vertex(g, i).
vertex(g, h).
vertex(g, g).
vertex(g, f).
vertex(g, e).
vertex(g, d).
vertex(g, c).
vertex(g, b).
vertex(g, a).

true.

?- list_arcs(g).
:- dynamic arc/4.

arc(g, d, f, 14).
arc(g, c, f, 4).
arc(g, i, g, 6).
arc(g, i, c, 2).
arc(g, i, h, 7).
arc(g, b, h, 11).
arc(g, a, h, 8).
arc(g, g, h, 1).
arc(g, g, f, 2).
arc(g, e, f, 10).
arc(g, e, d, 9).
arc(g, c, d, 7).
arc(g, c, b, 8).
arc(g, a, b, 4).

true.

NOTE: as you can see, by listing the arcs only arcs in one directions are shown.
that is because internally was more convenient in terms of memory using a single
assertion for representing a bidirectional arc.

?- list_graph(g).
:- dynamic vertex/2.

vertex(g, i).
vertex(g, h).
vertex(g, g).
vertex(g, f).
vertex(g, e).
vertex(g, d).
vertex(g, c).
vertex(g, b).
vertex(g, a).

:- dynamic arc/4.

arc(g, d, f, 14).
arc(g, c, f, 4).
arc(g, i, g, 6).
arc(g, i, c, 2).
arc(g, i, h, 7).
arc(g, b, h, 11).
arc(g, a, h, 8).
arc(g, g, h, 1).
arc(g, g, f, 2).
arc(g, e, f, 10).
arc(g, e, d, 9).
arc(g, c, d, 7).
arc(g, c, b, 8).
arc(g, a, b, 4).

true.


These two predicates will genereate lists of all the arcs/vertices members of g:

?- graph_arcs(G, Es).
Es = [arc(g,f,d,14),arc(g,f,c,4),arc(g,g,i,6),arc(g,c,i,2),arc(g,h,i,7),
      arc(g,h,b,11),arc(g,h,a,8),arc(g,h,g,1),arc(g,f,g,2),arc(g,f,e,10),
      arc(g,d,e,9),arc(g,d,c,7),arc(g,b,c,8),arc(g,b,a,4),arc(g,d,f,14),
      arc(g,c,f,4),arc(g,i,g,6),arc(g,i,c,2),arc(g,i,h,7),arc(g,b,h,11),
      arc(g,a,h,8),arc(g,g,h,1),arc(g,g,f,2),arc(g,e,f,10),arc(g,e,d,9),
      arc(g,c,d,7),arc(g,c,b,8),arc(g,a,b,4)].

Note: in this case, arcs in both directions are generated for more consistence.

?- graph_vertices(g, Vs).
Vs = [vertex(g,i),vertex(g,h),vertex(g,g),vertex(g,f),vertex(g,e),vertex(g,d),
      vertex(g,c),vertex(g,b),vertex(g,a)].

These predicates are bidirectional, and its results are independent from the
order in the list.


Now, if you want to get the lists of arcs starting from c or all of its directly
connected vertices:

?- vertex_neighbors(g, c, Es).
Es = [arc(g,c,i,2),arc(g,c,f,4),arc(g,c,d,7),arc(g,c,b,8)].

This works also in the opposite, regardless of the order in the list:

?- vertex_neighbors(G, V, [arc(g,c,i,2), arc(g,c,f,4), arc(g,c,b,8),
                           arc(g,c,d,7)]).
G = g,
V = c ;
false.

It is false because the interpreter works in order to find every possible
combination, taking advantage from the backtracking.


2.2.3 INTERACTING WITH THE FILE SYSTEM

If you want to save g to 'my_graph.csv', you can type:
write_graph(g, 'my_graph.csv').
which is equivalent to 
write_graph(g, 'my_graph.csv', graph).

If you only need to save few arcs, you can use the edges mode:
write_graph([arc(g,f,d,14), arc(g,f,c,4), arc(g,g,i,6], 'my_graph.csv', edges).
This will save the specified arc to the file.

Suppose you have the whole graph g in 'my_graph.csv'. If you want to load it in
the graph boh:
read_graph(boh, 'my_graph.csv').
This cleans out all the arcs and vertices contained in boh, and adds all the
arcs contain.
printing the graph will lead to:

?- list_graph(boh).
:- dynamic vertex/2.

vertex(boh, i).
vertex(boh, h).
vertex(boh, g).
vertex(boh, f).
vertex(boh, e).
vertex(boh, d).
vertex(boh, c).
vertex(boh, b).
vertex(boh, a).

:- dynamic arc/4.

arc(boh, d, f, 14).
arc(boh, c, f, 4).
arc(boh, i, g, 6).
arc(boh, i, c, 2).
arc(boh, i, h, 7).
arc(boh, b, h, 11).
arc(boh, a, h, 8).
arc(boh, g, h, 1).
arc(boh, g, f, 2).
arc(boh, e, f, 10).
arc(boh, e, d, 9).
arc(boh, c, d, 7).
arc(boh, c, b, 8).
arc(boh, a, b, 4).

true.



2.2.4 FINDING THE MINIMUM SPANNING TREE
Suppose to have the graph g created on point 2.2.1, and you need to build the
minimum spanning tree starting from the vertex a:

?- mst_prim(g, a).
true.

Then, you can traverse in pre-order the previously built tree, starting from any
source:

?- mst_get(g, c, PreorderTree).
PreorderTree = [arc(g, c, i, 2), arc(g, c, f, 4), arc(g, f, g, 2),
                arc(g, g, h, 1), arc(g, h, a, 8), arc(g, a, b, 4),
                arc(g, c, d, 7), arc(g, d, e, 9)].









--------------------------------------------------------------------------------
3. MINHEAP API 
--------------------------------------------------------------------------------
3.1 USAGE

3.1.1 HEAP MANIPULATION

new_heap(H).
this predicate adds the heap H to the knowledge base, if it doesn't exist. It is
always true.

delete_heap(H).
this predicate deletes the heap H and all of its entries. It is always true.

heap_insert(H, K, V).
this predicate inserts the entry with key K and value V (and it is true), only
if V is not already present in the heap.

heap_extract(H, K, V).
this predicate is true if the entry with key K and value V is removed from the
heap.

modify_key(H, NewKey, OldKey, V).
this predicate is true if the entry of H with key Oldkey and value V is replaced
by the entry with key NewKey and value V.



3.1.2 GETTING INFORMATIONS

heap_has_size(H, S).
this predicate is true if the heap H contains S elements.

heap_empty(H).
this predicate is true if the heap H has size 0

heap_not_empty(H)
the opposite of heap_empty/1.

heap_head(H, K, V).
this predicate is true if the element with the minimum key K has value V.

list_heap(H).
this predicate lists everything asserted about the heap H.
In particular, a heap is asserted as heap(H, S), where S is its size; every
entry of H is asserted as heap_entry(H, P, K, V), where P is the position, K is
its key and V is the associated value.




3.2 EXAMPLES

3.2.1 HEAP MANIPULATION

?- new_heap(h).
true.
this will add the heap H to the knowledge base.

If you want to insert the element with key 3 and value randomValue:
heap_insert(h, 3, randomValue).
true.

If you insert a second element with e key < 3, for example:
?- heap_insert(h, 2, anotherValue).
true.
the head will be moved at position 2 and the new element will become the head.

In fact, extracting from h will result in:
?- heap_extract(h, K, V).
K = 2,
V = anotherValue.

Now, in h there is only  the element containing randomValue. if you want to
change its key from 3 to 1, just run:
?- modify_key(h, 1, 3, randomValue).
true.


3.2.2 GETTING INFORMATIONS
Suppose you have the heap created at the previous point. You can retrieve its
head by running:
?- heap_head(h, K, V).
K = 2,
V = anotherValue.
You'll get the first element.

If you want to know h's size:
?- heap_has_size(h, S).
S = 2.

Or check if h is empty:
?- heap_empty(h).
false.
Similarly:
?- heap_not_empty(h).
true.

You can also list all the asserted dynamic facts representing h:
?- list_heap(h).
:- dynamic heap_entry/4.

heap_entry(h, 2, 3, randomValue).
heap_entry(h, 1, 2, anotherValue).

true.
