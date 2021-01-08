;;;; -*- Mode: Lisp -*-

;;;; mst.lisp --


;;;; Gabriele Canesi
;;;; 851637


;;;; GRAPHS API

(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))

;; The hash-table *vertices* has the following structure:
;;
;; Graph-id ---> another hash-table containing
;;                            key: vertex-id
;;                            value: vertex-rep (vertex graph-id vertex-id)
(defparameter *vertices* (make-hash-table :test #'equal))


;; The hash-table *arcs* has the following structure:
;;
;; Graph-id ---> another hash-table containing:
;;                            key: vertex-id
;;                            value: another hash-table containing:
;;                                          key: adjacent id to vertex-id
;;                                          value: arc weight
;; Arcs are stored in double direction: this allows to execute some functions
;; (for example graph-vertex-neighbors and, consequently, the prim'a  algorithm)
;; much faster.


(defparameter *arcs* (make-hash-table :test #'equal))


;; The hash-table *graphs* has the following structure:
;;
;; graph-id ---> graph-id
(defparameter *graphs* (make-hash-table :test #'equal))


(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  "If the graph doesn not exist, it creates it."
  (when (and (not (is-graph graph-id))
	     (not (null graph-id)))
    ;;create the hash-table which contains the graph-id  vertices
    (setf (gethash graph-id *vertices*) (make-hash-table
					 :test #'equal))
    
    ;;create the hash-table which contains the graph-id arcs
    (setf (gethash graph-id *arcs*) (make-hash-table
				     :test #'equal)))
  ;;create the hash table which contains the graph-id
  (setf (gethash graph-id *graphs*) graph-id))


(defun is-vertex (graph-id vertex-id)
  "Check if the vertex vertex-id is part of the graph graph-id"
  (if (is-graph graph-id)
      (gethash vertex-id (gethash graph-id *vertices*))))



(defun new-vertex (graph-id vertex-id)
  "If graph-id represents an existing graph, a new vertex with vertex-id is
added to it, if it does not exist. If the arc already exists, then its weight is
  updated to weight"
  (if (is-graph graph-id)
      (when (null (gethash vertex-id (gethash graph-id *vertices*)))
	;;these need to be initialized in order to create arcs without problems.
	;;create the vertex entry with key vertex-id and value vertex-rep
	(setf (gethash vertex-id (gethash graph-id *vertices*))
	      (list 'vertex graph-id vertex-id))
	
	;;create the neighbors hash table for storing arcs
	(setf (gethash vertex-id (gethash graph-id *arcs*))
	      (make-hash-table :test #'equal)))
      
      (error "The graph ~A does not exist" graph-id))
  (list 'vertex graph-id vertex-id))



(defun new-arc (graph-id v1-id v2-id &optional (weight 1))
  "If graph-id represents an existing graph, then v1-id and v2-id vertices are
created if they don't exist. Then, two arcs between v1-id and v2-id are created"
  (cond ((and (is-graph graph-id) (numberp weight) (>= weight 0))
	 ;;try creating the vertices
	 (new-vertex graph-id v2-id)
	 (new-vertex graph-id v1-id)
	 
	 ;;create the arcs in both direction, by adding the entries
	 ;;adjacent-id -> weight
	 (setf (gethash v2-id (gethash v1-id (gethash graph-id *arcs*)))
               weight)
	 (setf (gethash v1-id (gethash v2-id (gethash graph-id *arcs*)))
               weight)
	 (list 'arc graph-id v1-id v2-id weight))
	((is-graph graph-id)
	 (error "The weight ~A is not valid. Must be a non negative number"
		weight))
	(T (error "The graph ~A does not exist." graph-id))))


(defun graph-vertices (graph-id)
  "returns a list containing all of the vertices belonging to the graph
graph-id"
  (if (is-graph graph-id)
      (let ((vertex-list))
	(maphash (lambda (k v)
		   (declare (ignore k)) ;suppress the "not referenced" warning
		   (push  v vertex-list))
		 (gethash graph-id *vertices*))
	vertex-list)
      (error "The graph ~A does not exist." graph-id)))


;; Here visited is used for building a list containing only arcs in one
;; direction for a cleaner output, since all the graphs in this library are
;; undirected.
(defun graph-arcs (graph-id)
  "Returns a list containing all of the arcs belonging to graph-id, only in one
direction."
  (let ((arcs))
    (clrhash *visited*)
    
    ;; uses graph-vertices for retrieving all the vertices
    (mapcar (lambda (vertex)
	      
	      ;; here every vertex neighbor from graph-vertex-neighbor is
	      ;; scanned
	      (mapcar (lambda (neighbor)
			;; add the arc only if the target vertex has not been
			;; visited (which means that the arc in the opposite
			;; direction is not in the list)
			(if (not (is-visited (fourth neighbor)))
			    (push neighbor arcs)))
		      (graph-vertex-neighbors graph-id (third vertex)))
	      
	      
	      ;; this part check if there is a self loop, since
	      ;; graph-vertex-neighbors doesn't consider self loops.
	      (let ((self-loop-weight
		      (gethash (third vertex)
			       (gethash (third vertex)
					(gethash graph-id *arcs*)))))
		(if (not (null self-loop-weight))
		    (push (list 'arc (third vertex) (third vertex)
				self-loop-weight)
			  arcs))
		
		;; sets to visited the vertex, since all of its connected
		;; arcs have been added
		(set-visited (third vertex))))
	    (graph-vertices graph-id))
    arcs))

;; deleting a graphs deletes also the Prim's algorithm relative data.
(defun delete-graph (graph-id)
  "Removes the graph graph-id from the program data"
  (if (not (is-graph graph-id))
      (error "The graph ~A does not exist." graph-id))
  (remhash graph-id *arcs*)
  (remhash graph-id *vertices*)
  (remhash graph-id *graphs*)
  (remhash graph-id *vertex-keys*)
  (remhash graph-id *previous*)
  NIL)


(defun graph-vertex-neighbors (graph-id vertex-id)
  "Returns a list containing all of the arcs starting from the vertex vertex-id
in the graph graph-id, except for self loops"
  (cond ((is-vertex graph-id vertex-id)
	 (let* ((neighbors))
	   (maphash
	    (lambda (k v)
	      (if (not (equal vertex-id k)) ;exclude self loops here
		  (push (list 'arc graph-id vertex-id k v) neighbors)))
	    (gethash vertex-id (gethash graph-id *arcs*)))
	   neighbors))
	((is-graph graph-id) (error "The vertex ~A is not part of the graph ~A"
				    vertex-id graph-id))
	(T (error "The graph ~A does not exist" graph-id))))


(defun graph-vertex-adjacent (graph-id vertex-id)
  "Returns a list containing all of the vertices directly connected to vertex-id
in graph-id"
      (let ((adjacent))
	(mapcar (lambda (arc)
		  (push (list 'vertex graph-id (fourth arc)) adjacent))
		(graph-vertex-neighbors graph-id vertex-id))
	adjacent))


(defun graph-print (graph-id)
  "Prints a list of all the graph-id graph's arcs and vertices"
  (if (is-graph graph-id)
      (let ((arcs (graph-arcs graph-id))
	    (vertices (graph-vertices graph-id)))
	(print (append vertices arcs))
	graph-id)
      (error "The graph ~A does not exist" graph-id)))



;;; MINHEAPS API

;; non numeric keys are allowed. Two keys are compared lexicographically only if
;; one of those is not a number.


;; the created heaps are stored in this hash table, indexed by their id
(defparameter *heaps* (make-hash-table :test #'equal))


;; In this library, heaps are stored in structs with the following parameters:
(defstruct heap
  id
  size
  actual-heap
  positions-hash-table)

;; positions-hash-table traces the position of every value in order to retrieve
;; it in the actual-heap array. The heap head is stored at position 1.


;; under this heap / array size rate, heap array is cut
(defparameter heap-cut-limit (/ 1 2))

;; new / old actual-heap size after cut
(defparameter heap-cut-rate (/ 2 3))

;; when actual-heap is full, the size is multiplied by that parameter
(defparameter heap-increase-rate 2)
                                    
;; this is the default initial heap capacity if not specified in new-heap.
(defparameter heap-default-capacity 42)



(defun is-heap (heap-id)
  (not (null (gethash heap-id *heaps*))))




;; returns a new heap-rep given the id and the
;; updated &key parameters.
(defun make-updated-heap-rep (id &key size actual-heap positions-table)
  "Returns a new heap-rep with the specified key parameters updated"
  (make-heap
   :id id
   :size (if (null size)
	     (heap-size (gethash id *heaps*))
	     size)
   :actual-heap (if (null actual-heap)
		    (heap-actual-heap (gethash id *heaps*))
		    actual-heap)
   :positions-hash-table  (if (null positions-table)
			      (heap-positions-hash-table (gethash id *heaps*))
			      positions-table)))

(defun new-heap (heap-id &optional (capacity heap-default-capacity))
  "Creates a heap with an initial capacity"
  (if (and (numberp capacity)
	   (equal (ceiling capacity) capacity)
	   (>= capacity 0))
      (or (gethash heap-id *heaps*) 
	  (setf (gethash heap-id *heaps*)
		(make-heap
		 :id heap-id
		 :size 0
		 :actual-heap (make-array (+ 1 capacity) :adjustable T)
		 :positions-hash-table (make-hash-table :test #'equal))))
      (error "Capacity ~A is not allowed. Can only be a non negative integer"
	     capacity)))


(defun heap-delete (heap-id)
  (if (is-heap heap-id)
      (remhash heap-id *heaps*)
      (error "The heap ~A does not exist" heap-id))
  T)


(defun heap-empty (heap-id)
    (if (is-heap heap-id)
	(eq 0 (heap-size (gethash heap-id *heaps*)))
	(error "The heap ~A does not exist" heap-id)))


(defun heap-not-empty (heap-id)
  (not (heap-empty heap-id)))



(defun heap-head (heap-id)
  (if (heap-not-empty heap-id)
      (aref (heap-actual-heap (gethash heap-id *heaps*)) 1)
      (error "The heap ~A is empty" heap-id)))

(defun heap-full (heap-id)
  (if (is-heap heap-id)
      (equal (+ 1 (heap-size (gethash heap-id *heaps*)))
	     (length (heap-actual-heap(gethash heap-id *heaps*))))))


;; only numeric keys are allowed.
(defun heap-insert (heap-id k v)
  (if (and (is-heap heap-id)
	   (null (gethash v (heap-positions-hash-table
			     (gethash heap-id *heaps*)))))
      (let ((new-pos (+ 1 (heap-size (gethash heap-id *heaps*)))))
	
	;; if the array is full, multiplies its size
	(if (heap-full heap-id)
            (adjust-array (heap-actual-heap (gethash heap-id *heaps*))
			  (ceiling (* heap-increase-rate
				      (length (heap-actual-heap
					       (gethash heap-id *heaps*)))))))

        
	(setf (aref (heap-actual-heap (gethash heap-id *heaps*)) new-pos)
	      (list k v))
	(setf (gethash v (heap-positions-hash-table (gethash heap-id *heaps*)))
	      new-pos)
	(setf (gethash heap-id *heaps*) (make-updated-heap-rep heap-id
							       :size new-pos))
	(heap-swim heap-id new-pos) T)))



;; when an element is extracted, it is not really removed from the
;; actual-heap. Simply, since its position will be greater than heap-size, it
;; will not be part of the heap anymore, and when the heap size is
;; significantly less than the array length, then the array length is reduced
;; (according to the previously defined parameters)
(defun heap-extract (heap-id)
  (let ((head (heap-head heap-id)))
    (setf (gethash heap-id *heaps*)
	  (make-updated-heap-rep heap-id
				 :size (- (heap-size (gethash heap-id *heaps*))
 					  1)))
    
    (heap-swap heap-id 1 (+ 1 (heap-size (gethash heap-id *heaps*))))

    ;; resize the array if needed
    (if (< (/ (heap-size (gethash heap-id *heaps*))
	      (length (heap-actual-heap (gethash heap-id *heaps*))))
	   heap-cut-limit)
	
	(adjust-array (heap-actual-heap (gethash heap-id *heaps*))
		      (ceiling (* (length (heap-actual-heap
					   (gethash heap-id *heaps*)))
				  heap-cut-rate))))
    
    (heap-sink heap-id 1)
    head))



(defun heap-modify-key (heap-id new-key old-key value)
  (let ((heap-pos (heap-position heap-id value old-key)))
    (when (not (null heap-pos))
          (setf (aref (heap-actual-heap (gethash heap-id *heaps*)) heap-pos)
		(list new-key value))
	  
          (cond ((< old-key new-key)
                 (heap-sink heap-id heap-pos))
                ((> old-key new-key)
                 (heap-swim heap-id heap-pos)))
          T)))

(defun heap-print (heap-id)
  (when (not (null (gethash heap-id *heaps*)))
    ;; prints only the elements which are actually part of the heap
    (format t "SIZE: ~A, ~%ELEMENTS: ~D%"
            (heap-size (gethash heap-id *heaps*))
            (subseq (heap-actual-heap (gethash heap-id *heaps*))
		    1 (+ 1 (heap-size (gethash heap-id *heaps*)))))
    T))


(defun heap-position (heap-id value &optional (key nil))
  ;; retrieve the position by accessing the hash-table containing the
  ;; value-position relationships
  (let ((pos (if (not (null (gethash heap-id *heaps*)))
		 (gethash value (heap-positions-hash-table
				 (gethash heap-id *heaps*))))))
    (if (not (null pos))
	(if (null key)
	    pos
	    (if (equal key (first (aref (heap-actual-heap
					 (gethash heap-id *heaps*))
					pos)))
		pos)))))

(defun heap-sink (heap-id pos)
  (if (>= pos 1)
      
      ;; check which position contains the minimum key between pos, its left
      ;; child and its right child.
      (let* ((left (* 2 pos))
             (right (+ 1 left))
             (left-key (if (>= (heap-size (gethash heap-id *heaps*)) left)
                           (first (aref (heap-actual-heap
					 (gethash heap-id *heaps*))
					left))))
             (right-key (if (>= (heap-size (gethash heap-id *heaps*)) right)
                            (first (aref (heap-actual-heap
					  (gethash heap-id *heaps*))
					 right))))
             (pos-key (first (aref (heap-actual-heap
				    (gethash heap-id *heaps*))
				   pos)))
             (min-pos (cond ((>= (heap-size (gethash heap-id *heaps*)) right)
			     (cond ((and (key<=  right-key left-key)
					 (key< right-key pos-key))
				    right)  
				   ((and (key<= left-key right-key)
					 (key< left-key pos-key))
				    left)  
				   (T pos)))
                            ((equal (heap-size (gethash heap-id *heaps*)) left)
			     (cond ((key< left-key pos-key) left)  
				   (T pos))) 
                            (T pos))))
	
	;; if the minimum key is left-key or right-key, then the element at
	;; min-pos is swapped with the minimum and this function is called on
	;; that child.
        (when (not (equal pos min-pos))
          (heap-swap heap-id pos min-pos)
          (heap-sink heap-id min-pos)))))


(defun key< (a b)
  (cond ((and (numberp a) (numberp b))
	 (< a b))
	(T (string< (princ-to-string a) (princ-to-string b)))))

(defun key<= (a b)
  (or (equal a b)
      (key< a b)))

(defun heap-swim (heap-id p)
  (if (and (> p 1) (<= p (heap-size (gethash heap-id *heaps*))))
      (let ((parent (floor p 2)))
        (when (key< (first
		  (aref (heap-actual-heap (gethash heap-id *heaps*)) p))
		 (first
		  (aref (heap-actual-heap (gethash heap-id *heaps*)) parent)))
          (heap-swap heap-id p parent)
          (heap-swim heap-id parent)))))


(defun heap-swap (heap-id pos1 pos2)

  ;; removes the position informations from the hash table
  (remhash (second (aref (heap-actual-heap (gethash heap-id *heaps*)) pos1))
	   (heap-positions-hash-table (gethash heap-id *heaps*)))   
  (remhash (second (aref (heap-actual-heap (gethash heap-id *heaps*)) pos2))
	   (heap-positions-hash-table (gethash heap-id *heaps*)))
  
  ;; put in the potiions hash table the new values positions, only if they are
  ;; still part of the heap (heap-size >= position)
  (if (<= pos1 (heap-size (gethash heap-id *heaps*)))
      (setf (gethash (second (aref (heap-actual-heap (gethash heap-id *heaps*))
				   pos2))
		     (heap-positions-hash-table (gethash heap-id *heaps*)))
	    pos1))
  
  (if (<= pos2 (heap-size (gethash heap-id *heaps*)))
      (setf (gethash (second (aref (heap-actual-heap (gethash heap-id *heaps*))
				   pos1))
		     (heap-positions-hash-table (gethash heap-id *heaps*)))
	    pos2))
  
  ;; swap the elements in the actual heap
  (rotatef (aref (heap-actual-heap (gethash heap-id *heaps*)) pos1)
	   (aref (heap-actual-heap (gethash heap-id *heaps*)) pos2)))

;;;; MINIMUM SPANNING TREE

;; graph-mst-temp is used during the mst-get execution. Didn't know if passing a
;; hash table in a function and doing on it some assignements was legit, so i
;; put it here.
(defparameter *graph-mst-temp* (make-hash-table :test #'equal))


;; mst-heap is the id of the created heap during the Prim's algorithm execution
(defparameter mst-heap 'mst-heap)



(defun mst-prim (graph-id source)
  "This function runs the Prim's algorithm starting from the vertex with id
source. in the graph graph-id"
  (when (is-vertex graph-id source)
    (mst-init graph-id source mst-heap)
    (mst-prim-rec graph-id mst-heap)
    (heap-delete mst-heap))
  NIL)


;; this is the main part of the Prim's algorithm implementation.
;; the algorithm stops if the heap is empty or if the extracted element has
;; MOST-POSITIVE-DOUBLE-FLOAT key.
(defun mst-prim-rec (graph-id heap-id)
  (if (heap-not-empty heap-id)

      ;; extract the element and save in head
      (let* ((head (heap-extract heap-id))
             (head-id (third (second head))))

	;; finding a MOST-POSITIVE-DOUBLE-FLOAT key means that the graph is not
	;; connected to the source at this point, so it stops.
	(when (< (first head)  MOST-POSITIVE-DOUBLE-FLOAT)

	  ;; save the head key in the vertex-keys hash table
	  (setf (gethash (third (second head)) (gethash graph-id *vertex-keys*))
		(first head))

	  ;; maps all the head's neighbors and updates the arcs weight
	  (mapcar
	   (lambda (arc)
	     (let ((pos (heap-position
			 heap-id (list 'vertex graph-id (fourth arc)))))

	       ;; Update a vertex key with the arc weight which connects it to
	       ;; the extracted element only if it is not already part of the
	       ;; minimum spanning tree and its key is lower than the weight.
	       (when (and (not (null pos))
			  (> (first (aref (heap-actual-heap
					   (gethash heap-id *heaps*))
					  pos))
			     (fifth arc)))
		 (heap-modify-key heap-id (fifth arc)
				  (first (aref  (heap-actual-heap
						 (gethash heap-id *heaps*))
						pos))
				  (second (aref (heap-actual-heap
						 (gethash heap-id *heaps*))
						pos)))
		 
		 (setf (gethash (fourth arc) (gethash graph-id *previous*))
		       head-id))))
	   (graph-vertex-neighbors graph-id head-id))

	  (mst-prim-rec graph-id heap-id)))))




;; Cleans the vertex-key and previous data relative to graph-id and adds tho
;; the mst-heap all the graph's vertices with key the constant
;; MOST-POSITIVE-DOUBLE-FLOAT, except for the source, which has key 0.

(defun mst-init (graph-id source heap-id)
  (let ((vertices (graph-vertices graph-id)))
    
    ;; reset the needed data structures
    (if (is-heap heap-id)
	(heap-delete heap-id))
    (remhash graph-id *vertex-keys*)
    (remhash graph-id *previous*)
    (setf (gethash graph-id *previous*) (make-hash-table :test #'equal))
    (setf (gethash graph-id *vertex-keys*) (make-hash-table :test #'equal))
    (new-heap heap-id (list-length vertices))
    
    ;; init each vertex key
    (mapcar (lambda (vertex)
              (heap-insert heap-id MOST-POSITIVE-DOUBLE-FLOAT vertex)
              (setf (gethash (third vertex) (gethash graph-id *vertex-keys*))
		    MOST-POSITIVE-DOUBLE-FLOAT))
            vertices)

    ;; change the source's key from MOST-POSITIVE-DOUBLE-FLOAT to 0
    (heap-modify-key heap-id 0 MOST-POSITIVE-DOUBLE-FLOAT
		     (list 'vertex graph-id source))
    (setf (gethash source (gethash graph-id *vertex-keys*)) 0)))


(defun mst-vertex-key (graph-id vertex-id)
  "Returns the arc weight which connects the vertex vertex-id to its previous"
  (if (not (null (gethash graph-id *vertex-keys*)))
      (gethash vertex-id (gethash graph-id  *vertex-keys*))))


(defun mst-previous (graph-id vertex-id)
  "Returns the id of the vertex-id vertex in the graph-id's minimum spanning
tree if it exists"
  (if (not (null (gethash graph-id *previous*)))
      (gethash vertex-id (gethash graph-id *previous*))))


;; Calls mst-get-rec on the source is it exists, after setting up *visited*
;; and *graph-mst-temp* hash tables, and building a new graph equivalent to
;; the minimum spanning tree.
(defun mst-get (graph-id source)
  (when (is-vertex graph-id source)
    (clrhash *visited*)
    (clrhash *graph-mst-temp*)
    (build-mst-graph graph-id)
    (let ((tree (mst-get-rec graph-id source)))
      (clrhash *graph-mst-temp*)
      tree)))



;; this function is called on every mst vertex in order to find its not
;; visited neighbors and sort them, before calling mst-get-list.
(defun mst-get-rec (graph-id source)
  (set-visited source)
  (mst-get-list graph-id
                (stable-sort
                 (stable-sort
		  ;; treat numbers as strings for mantaining the right order
                  (free-arcs graph-id source) (lambda (a b)
						(string< (princ-to-string a)
							 (princ-to-string b)))
		  :key 'fourth)
                 '< :key 'fifth)))



;; This function is used to build a new temporary graph, which is the
;; corresponding mst to be visited. This implementation choice uses more
;; memory, but it results a lot faster since only two linear scans
;; are done.

(defun build-mst-graph (graph-id)
  (let ((vertices (graph-vertices graph-id)))

    ;; creates one hash table for each vertex in order to prepare the arcs
    ;; creation
    (mapcar (lambda (vertex)
	      (setf (gethash (third vertex) *graph-mst-temp*)
		    (make-hash-table :test #'equal)))
	    vertices)

    ;; scans all the previous relationship for creating double directed arcs in
    ;; the graph-mst-temp hash table
    (mapcar (lambda (vertex)
	      (let ((previous (mst-previous graph-id (third vertex))))
		(when (not (null previous))
		  (setf (gethash (third vertex)
				 (gethash previous *graph-mst-temp*))
			(mst-vertex-key graph-id (third vertex)))
		  
		  (setf (gethash previous (gethash (third vertex)
						   *graph-mst-temp*))
			(mst-vertex-key graph-id (third vertex))))))
	    vertices)))




(defun mst-get-list (graph-id arcs)
  (if (not (null arcs))
      (cons (car arcs)
	    ;; calls mst-get-rec on every arc an it appends the result of the
	    ;; rest of the list.
            (append (mst-get-rec graph-id (fourth (car arcs)))
                    (mst-get-list graph-id (cdr arcs))))))


;; return a list of not visited source neighbors in the tree.
(defun free-arcs (graph-id source)
  (let ((arcs-list))
    (maphash (lambda (k v)
               (if (not (is-visited k))
                   (push  (list 'arc graph-id source k v) arcs-list)))
             (gethash source *graph-mst-temp*))
    arcs-list))


(defun is-visited (vertex)
  (not (null (gethash vertex *visited*))))

(defun set-visited (vertex)
  (setf (gethash vertex *visited*) vertex))


;;;; end of file -- mst.lisp --


(defun test ()
  (new-graph 'g)
  (new-vertex 'g 'a)
  (new-vertex 'g 'b)
  (new-vertex 'g 'c)
  (new-vertex 'g 'd)
  (new-vertex 'g 'e)
  (new-vertex 'g 'f)
  (new-vertex 'g 'g)
  (new-vertex 'g 'h)
  (new-vertex 'g 'i)
  (new-arc 'g 'a 'b 4)
  (new-arc 'g 'c 'b 8)
  (new-arc 'g 'c 'd 7)
  (new-arc 'g 'd 'e 9)
  (new-arc 'g 'e 'f 10)
  (new-arc 'g 'f 'g 2)
  (new-arc 'g 'g 'h)
  (new-arc 'g 'a 'h 8)
  (new-arc 'g 'g 'i 6)
  (new-arc 'g 'i 'h 7)
  (new-arc 'g 'h 'b 11)
  (new-arc 'g 'd 'f 14)
  (new-arc 'g 'c 'f 4)
  (new-arc 'g 'c 'i 2))



(defun prim-and-get (g v1 v2)
  (mst-prim g v1)
  (somma-giusta (mst-get g v2)))




(defun somma-giusta (lista)
  (if (not (null lista))
      (+ (somma-giusta (cdr lista)) (fifth (car lista)))
      0))







(defun somma-archi (graph-id)
  (let ((somma 0))
    (maphash (lambda (k v)
	       (declare (ignore k))
               (setf somma (+ somma v)))
             (gethash graph-id *vertex-keys*))
    somma))


(defun print-hash (hash-table)
  (maphash (lambda (k v) (print (list k v))) hash-table))

;(load "/Users/gabrielecanesi/Downloads/lisptest/primkiller_10k.LISP")




(defun test-mix ()
  (new-graph 'm)
  (new-vertex 'm 'a)
  (new-vertex 'm 'b)
  (new-vertex 'm '1)
  (new-vertex 'm "1")
  (new-vertex 'm 'c)
  (new-vertex 'm '2)
  (new-vertex 'm "2")
  (new-arc 'm 'a 'b)
  (new-arc 'm 'a "1")
  (new-arc 'm 'a 1)
  (new-arc 'm 'a 'c)
  (new-arc 'm 'a "2")
  (new-arc 'm 'c 'b)
  (new-arc 'm 'c "1")
  (new-arc 'm 'c 2)
  (new-arc 'm '2 "2")
  (new-arc 'm 'a 10))


(defun un-test ()
  (new-graph 'u)
  (new-arc 'u 'a 'b)
  (new-arc 'u 'c 'a)
  (new-arc 'u 'c 'b)
  (new-arc 'u 'd 'e)
  (new-arc 'u 'd 'f)
  (new-arc 'u 'e 'f))

(ql:quickload "cl-csv")
(require "cl-csv")

(defun kill ()
  (new-graph 'kill)
  (cl-csv:read-csv #P"~/primkiller_definitivo.csv"
                   :separator #\Tab
                   :map-fn #' (lambda (row)
                                (new-arc 'kill
                                         (read-from-string (first row))
                                         (read-from-string (second row))
                                         (parse-integer (third row)))))
  nil)
