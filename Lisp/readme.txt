Gabriele Canesi
Mat 851637


********************************************************************************
PROJECT: MANIPULATION OF GRAPHS AND HEAPS TO GENERATE ITS MINIMUM
SPANNING TREES
********************************************************************************

INTRODUCTION
This project contains mainly two libraries: Graphs and MinHeap.
Combining these two APIs, it is possible to find the Minimum spanning tree of
undirected graphs, implementing the Prim's algorithm.

Prerequisites: Installed Common Lisp compiler.
Tested on sbcl 2.0.11


--------------------------------------------------------------------------------
GRAPHS API
--------------------------------------------------------------------------------
You can use this API for creating, manipulating and getting informations about
any directed graph.

USAGE

GRAPHS/VERTICES/ARCS MANIPULATION:

Creating a graph:
(new-graph graph-id) -> graph-id:
this function adds the graph graph-id it to the program data.

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



GETTING INFORMATIONS ABOUT GRAPHS

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


MINIMUM SPANNING TREE
In order to generate and retrieve a graph MST, there are two main functions:

(mst-prim graph-id vertex-id) -> NIL:
this function builds a hash table containing the parent relationships between
vertices, and another hash table in which to every vertex there is associated
the weight of the arc which connects it to the tree.

(mst-get graph-id vertex-id) -> preorder-mst:
this function returns the list of the tree arcs traversed in preorder, using the
output produced by mst-prim.






--------------------------------------------------------------------------------
MINHEAP API
--------------------------------------------------------------------------------

The implementation of this library is independent from the graphs API, and it is
used for the execution of the Prim'a Algorithm.



