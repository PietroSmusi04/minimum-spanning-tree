Gabriele Canesi
Mat 851637


********************************************************************************
MANIPULATION OF GRAPHS AND HEAPS TO GENERATE ITS MINIMUM
SPANNING TREES
********************************************************************************

Summary
1. INTRODUCTION
2. GRAPHS API
    2.1 USAGE
        2.1.1 GRAPHS MANIPULATION
        2.1.2 GETTING GRAPHS INFORMATIONS
        2.1.3 FINDING THEMINIMUM SPANNING TREE
    2.2 EXAMPLES
        2.2.1 GRAPHS MANIPULATION
        2.2.2 GETTING INFORMATIONS
        2.2.3 FINDING THE MINIMUM SPANNING TREE
3. HEAPS API
    3.1 USAGE
        3.1.1 HEAPS MANIPULATION
        3.1.2 QUERYING HEAPS INFORMATIONS
    3.2 EXAMPLES


1. INTRODUCTION
This project contains mainly two libraries: Graphs and MinHeap.
Combining these two APIs, it is possible to find the minimum spanning tree of
undirected graphs, by using the Prim's algorithm.

Prerequisites: Installed Common Lisp compiler.
Tested on sbcl 2.0.11


--------------------------------------------------------------------------------
2. GRAPHS API
--------------------------------------------------------------------------------
You can use this API for creating, manipulating and getting informations about
any directed graph.

2.1 USAGE

2.1.1 GRAPHS MANIPULATION

Creating a graph:
(new-graph graph-id) -> graph-id:
this function adds the graph graph-id to the program data.

Deleting a graph:
(delete-graph graph-id) -> NIL:
this function deletes the graph graph-id and all its vertices and arcs.

Creating a vertex:
(new-vertex graph-id vertex-id) -> vertex-rep:
this function adds a new vertex to the graph graph-id.
Vertices are represented as lists:
(vertex graph-id vertex-id)


Creating an arc:
(new-arc graph-id vertex1-id vertex2-id &optional (weight 1)) -> arc-rep:
this function connects two vertices with a weighted arc. If those vertices don't
exist, then it creates them.
Arcs are represented as lists:
(arc graph-id vertex1-id vertex2-id weight)
NOTE: Self loops are not created. This choice has been taken because this
project is mainly meant to find the minimum spanning trees, and self loops are
irrilevant for this purpose.


2.1.2 GETTING INFORMATIONS ON GRAPHS

Checking the existence of a graph:
(is-graph graph-id) -> graph-id or NIL:
this function returns graph-id if the graph exist, else it returns NIL.

Getting all the vertices in a graph:
(graph-vertices graph-id) -> vertex-rep-list:
this function returns the vertices list belonging to the graph graph-id.

Getting all the arcs in a graph:
(graph-arcs graph-id) -> arc-rep-list:
this function returns the arcs list belonging to the graph graph-id.

Getting lists of arcs/verices connected to a vertex:
(graph-vertex-neighbors graph-id vertex-id) -> arc-rep-list:
this function returns all the arcs starting from the vertex vertex-id.

(graph-vertex-adjacent graph-id vertex-id) -> vertex-rep-list:
this function returns all the vertices directly connected to thr vertex
vertex-id.

Printing the graph
(graph-print graph-id) -> graph-id:
this function prints out the graph-id's verttices and arcs.


2.1.3 FINDING THE MINIMUM SPANNING TREE
In order to generate and retrieve a graph MST, there are two main functions:

(mst-prim graph-id vertex-id) -> NIL:
this function builds a hash table containing the parent relationships between
vertices, and another hash table in which to every vertex there is associated
the weight of the arc which connects it to the tree. If the graph graph-id  is
not connected, then only the connected part to Source is built.
During and after the execution, every vertex has a vertex key, which is the
weight of the arcs which connect it to the tree, and a previous vertex, which is
the parent in the tree. If you want to check this parameters:
(mst-vertex-key graph-id vertex-id) -> key
(mst-previous graph-id vertex-id) -> previous-id

(mst-get graph-id vertex-id) -> preorder-mst:
this function returns the list of the tree arcs traversed by preorder, using the
output produced by mst-prim. The exiting arcs from a vertex are sorted by
weight, and if it's the same, then they are sorted by the connected vertex name.


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

First, create a graph:
CL-USER> (new-graph 'g)
G

Now you can add the vertices:
CL-USER> (new-vertex 'g 'a)
(VERTEX G A)
T
CL-USER> (new-vertex 'g 'b)
(VERTEX G B)
T
CL-USER> (new-vertex 'g 'c)
(VERTEX G C)
T
CL-USER> (new-vertex 'g 'd)
(VERTEX G D)
T
CL-USER> (new-vertex 'g 'E)
(VERTEX G E)
T
CL-USER> (new-vertex 'g 'f)
(VERTEX G F)
T
CL-USER> (new-vertex 'g 'g)
(VERTEX G G)
T
CL-USER> (new-vertex 'g 'h)
(VERTEX G H)
T
CL-USER> (new-vertex 'g 'i)
(VERTEX G I)
T

Now, let's connect them:

CL-USER> (new-arc 'g 'a 'b 4)
(ARC G A B 4)
CL-USER> (new-arc 'g 'a 'h 8)
(ARC G A H 8)
CL-USER> (new-arc 'g 'b 'c 8)
(ARC G B C 8)
CL-USER> (new-arc 'g 'b 'h 11)
(ARC G B H 11)
CL-USER> (new-arc 'g 'h 'g)    ;it is the same as (new-arc 'g 'h 'g 1)
(ARC G H G 1)
CL-USER> (new-arc 'g 'g 'f 2)
(ARC G G F 2)
CL-USER> (new-arc 'g 'f 'c 4)
(ARC G F C 4)
CL-USER> (new-arc 'g 'c 'i 2)
(ARC G C I 2)
CL-USER> (new-arc 'g 'i 'h 7)
(ARC G I H 7)
CL-USER> (new-arc 'g 'i 'g 6)
(ARC G I G 6)
CL-USER> (new-arc 'g 'c 'd 7)
(ARC G C D 7)
CL-USER> (new-arc 'g 'd 'e 9)
(ARC G D E 9)
CL-USER> (new-arc 'g 'd 'f 14)
(ARC G D F 14)
CL-USER> (new-arc 'g 'e 'f 10)
(ARC G E F 10)


If you don't need anymore g, then:
CL-USER> (delete-graph 'g)
NIL


2.2.2 GETTING GRAPHS INFORMATIONS
Assuming g as the previous graph, you can get its vertices list:
CL-USER> (graph-vertices 'g)
((VERTEX G A) (VERTEX G B) (VERTEX G C) (VERTEX G D) (VERTEX G E) (VERTEX G F)
 (VERTEX G G) (VERTEX G H) (VERTEX G I))

Similarly, you can require its list of arcs:
CL-USER> (graph-arcs 'g)
((ARC I C 2) (ARC I H 7) (ARC I G 6) (ARC H B 11) (ARC H I 7) (ARC H A 8)
 (ARC H G 1) (ARC G I 6) (ARC G H 1) (ARC G F 2) (ARC F C 4) (ARC F D 14)
 (ARC F G 2) (ARC F E 10) (ARC E F 10) (ARC E D 9) (ARC D F 14) (ARC D E 9)
 (ARC D C 7) (ARC C I 2) (ARC C F 4) (ARC C D 7) (ARC C B 8) (ARC B H 11)
 (ARC B C 8) (ARC B A 4) (ARC A H 8) (ARC A B 4))

If you need it, you can print all of its vertices and arcs:
CL-USER> (graph-print 'g)

G 
(VERTEX G A) 
(VERTEX G B) 
(VERTEX G C) 
(VERTEX G D) 
(VERTEX G E) 
(VERTEX G F) 
(VERTEX G G) 
(VERTEX G H) 
(VERTEX G I) 
(ARC A B 4) 
(ARC A H 8) 
(ARC B A 4) 
(ARC B C 8) 
(ARC B H 11) 
(ARC C B 8) 
(ARC C D 7) 
(ARC C F 4) 
(ARC C I 2) 
(ARC D C 7) 
(ARC D E 9) 
(ARC D F 14) 
(ARC E D 9) 
(ARC E F 10) 
(ARC F E 10) 
(ARC F G 2) 
(ARC F D 14) 
(ARC F C 4) 
(ARC G F 2) 
(ARC G H 1) 
(ARC G I 6) 
(ARC H G 1) 
(ARC H A 8) 
(ARC H I 7) 
(ARC H B 11) 
(ARC I G 6) 
(ARC I H 7) 
(ARC I C 2) 
NIL


If you want to get the list of starting arcs from c:
CL-USER> (graph-vertex-neighbors 'g 'c)
((ARC G C I 2) (ARC G C F 4) (ARC G C D 7) (ARC G C B 8))

Similarly, for getting the list of adjacent vertices:
CL-USER> (graph-vertex-adjacent 'g 'c)
((VERTEX G B) (VERTEX G D) (VERTEX G F) (VERTEX G I))


2.2.3 FINDING THE MINIMUM SPANNING TREE
First of all, in order to build the tree from a starting vertex, for example a,
you must run:
CL-USER> (mst-prim 'g 'a)
NIL

Then, you can get the list of traversed arcs in pre-order by any vertex (in this
case c):

CL-USER> (mst-get 'g 'c)
((ARC G C I 2) (ARC G C F 4) (ARC G F G 2) (ARC G G H 1) (ARC G H A 8)
 (ARC G A B 4) (ARC G C D 7) (ARC G D E 9))




--------------------------------------------------------------------------------
3. MINHEAP API
--------------------------------------------------------------------------------


3.1 USAGE

3.1.1 HEAP MANIPULATION

(new-heap heap-id &optional (capacity 42)) -> heap-rep:
this function creates a new struct and it makes it accessible by calling the
API's functions on the heap-id. If no capacity is specified, then it is set to
42. It returns its struct containing id, size, array of elements and a hash
table used for finding the elements positions in constant time.

(heap-insert heap-id K V) -> boolean:
It adds the element with key K and value V to the heap, only if V is not in the
heap. The return value depends on the success of that operation. If the heap
size is greater than the array capacity, then the capacity is increased by 15.

(heap-extract heap-id) -> (list K V):
this function removes the entry with the minimum key and it returns it.

(heap-modify-key heap-id new-key old-key V) -> boolean:
this function searches the entry (old-key V) and it replaces it with
(new-key V). The return value depends on the success or not.



3.1.2 GETTING INFORMATIONS

(heap-empty heap-id) -> boolean:
this function returns true if the heap identified as heap-id does not contains
elements.

(heap-head heap-id) -> (list K V):
this function returns the entry with the minimum key, contained in the heap-rep
indexed as heap-id.

(heap-print heap-id) -> NIL
this function prints the heap-rep indexed as heap-id.



3.2 EXAMPLES

3.2.1 HEAP MANIPULATION

If you want to create a heap identified as h, then write:
CL-USER> (new-heap 'h)
#S(HEAP
   :ID H
   :SIZE 0
   :ACTUAL-HEAP #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                  0 0 0 0 0 0 0 0 0 0 0 0)
   :POSITIONS-HASH-TABLE #<HASH-TABLE :TEST EQUAL :COUNT 0 {1034D91433}>)


If you want to add some elements to it, for example the entries key-value
(42, 'my-value), (30, 'another-value):
CL-USER> (heap-insert 'h 42 'my-value)
T


Now, if you want to change its key from 42 to 2:
CL-USER> (heap-modify-key 'h 2 42 'my-value)
T


Extracting will result in:
CL-USER> (heap-extract 'h)
(2 MY-VALUE)



3.2.2 GETTING INFORMATIONS

Assume you have already created the heap h described at the previous point.
You wonder if it is empty. In that case:
CL-USER> (heap-empty 'h)
NIL

It is also possible getting the head without extracting it:
CL-USER> (heap-head 'h)
(42 MY-VALUE)


Now, print the heap:
CL-USER> (heap-print 'h)
SIZE: 2,
ELEMENTS: #((30 ANOTHER-VALUE) (42 MY-VALUE))%
NIL
