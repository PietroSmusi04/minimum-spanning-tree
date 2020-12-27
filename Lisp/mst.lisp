;;;; Gabriele Canesi
;;;; 851637

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))


;; graph-mst-temp is used during the mst-get execution. Didn't know if passing a
;; hash table in a function and doing on it some assignements was legit, so i put
;; it here.
(defparameter *graph-mst-temp* (make-hash-table :test #'equal))

;; mst-heap is the id of the created heap during the Prim's algorithm execution
(defparameter mst-heap 'mst-heap)

(defstruct heap
  id
  size
  actual-heap
  positions-hash-table)


(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (or (is-graph graph-id)
      (progn
        (setf (gethash graph-id *arcs*) (make-hash-table :test #'equal))
        (setf (gethash graph-id *graphs*) graph-id))))

(defun is-vertex (graph-id vertex-id)
  (gethash (list 'vertex graph-id vertex-id) *vertices*))


;;;; when creating a vertex, also the hash table indexed by vertex-id, in order
;;;; to create arcs later.
(defun new-vertex (graph-id vertex-id)
  (if (null (gethash (list 'vertex graph-id vertex-id) *vertices*))
      (progn
        (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
              (list 'vertex graph-id vertex-id))
        (setf (gethash vertex-id (gethash graph-id *arcs*))
              (make-hash-table :test #'equal))))
  (gethash (list 'vertex graph-id vertex-id) *vertices*))



;;;; arcs in both directions are created for retrieving informations on
;;;; connected objects to a vertex much faster. the structure of every arc is
;;;; the following:
;;;;                                     HASH TABLE
;;;;    V                                  V                     HASH-TABLE
;;;;                                                        K                 V
;;;; graph-id-->     (vertex-id-->                    (adjacent vertex-id -> weight))
;;;;                  other vertex-ids...                          other adjacents.....
;;;; 

(defun new-arc (graph-id v1-id v2-id &optional (weight 1))
  (if (and (not (equal v1-id v2-id)) (>= weight 0))
      (progn
        (if (is-graph graph-id)
            (progn
              (new-vertex graph-id v2-id)
              (new-vertex graph-id v1-id)
              (setf (gethash v2-id (gethash v1-id (gethash graph-id *arcs*)))
                    weight)
              (setf (gethash v1-id (gethash v2-id (gethash graph-id *arcs*)))
                    weight)))
        (list 'arc graph-id v1-id v2-id weight))
      (progn
        (print "Self loops and negative weights are not allowed.")
        NIL)))


(defun graph-vertices (graph-id)
  (let ((vertex-list))
    (maphash (lambda (k v)
               (if (equal (second k) graph-id)
                   (push  (list 'vertex graph-id (third k)) vertex-list)))
             *vertices*)
    vertex-list))


(defun graph-arcs (graph-id)
  (let ((arcs))
    (maphash (lambda (k v)
               (maphash (lambda (k1 v1)
                          (push (list 'arc k k1 v1) arcs))
                        v))
             (gethash graph-id *arcs*))
    arcs))

(defun delete-graph (graph-id)
  (remhash graph-id *arcs*)
  (maphash (lambda (k v)
             (if (equal graph-id (second k))
                 (remhash k *vertices*)) NIL) *vertices*)
  (remhash graph-id *graphs*)
  (remhash graph-id *graph-mst-temp*))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (let* ((neighbors))
      (maphash
       (lambda (k v)
         (push (list 'arc graph-id vertex-id k v) neighbors))
       (gethash vertex-id (gethash graph-id *arcs*)))
  neighbors))


(defun graph-vertex-adjacent (graph-id vertex-id)
  (let ((adjacent))
    (mapcar (lambda (arc)
              (push (list 'vertex graph-id (fourth arc)) adjacent))
            (graph-vertex-neighbors graph-id vertex-id))
    adjacent))



(defun graph-print (graph-id)
  (if (is-graph graph-id)
      (progn
        (print graph-id)
        (maphash (lambda (k v)
                   (if (equal (second k) graph-id)
                       (print v)))
                 *vertices*)
        (maphash (lambda (k v)
                   (maphash (lambda (k1 v1)
                              (print (list 'arc k k1 v1)))
                            v))
                 (gethash graph-id *arcs*)))))


(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*) 
      (setf (gethash heap-id *heaps*)
            (make-heap
             :id heap-id
             :size 0
             :actual-heap (make-array (+ 1 capacity) :adjustable T)
             :positions-hash-table (make-hash-table :test #'equal)))))


(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))


(defun heap-empty (heap-id)
  (eq 0 (heap-size (gethash heap-id *heaps*))))

(defun heap-not-empty (heap-id)
  (not (heap-empty heap-id)))



(defun heap-position (heap-id value &optional (key nil))
  (let ((heap-rep (gethash heap-id *heaps*)))
    (if (not (null (gethash value (heap-positions-hash-table heap-rep))))
	(let ((pos))
          (if (null key)
              (setf pos (gethash value (heap-positions-hash-table heap-rep)))
              (if (equal
                   (first (aref (heap-actual-heap heap-rep)
				(gethash value
					 (heap-positions-hash-table heap-rep))))
                   key)
                  (setf pos
			(gethash value (heap-positions-hash-table heap-rep)))))
          pos))))

(defun heap-sink (heap-id pos)
  (let ((heap-rep (gethash heap-id *heaps*)))
  (if (>= pos 1)
      (let* ((left (* 2 pos))
             (right (+ 1 left))
             (size (heap-size heap-rep))
             (heap-array (heap-actual-heap heap-rep))
             (left-key (if (>= size left)
                           (first (aref heap-array left))
                           NIL))
             (right-key (if (>= size right)
                            (first (aref heap-array right))
                            NIL))
             (pos-key (first (aref heap-array pos)))
             (min-pos (cond
                        ((>= size right) (cond 
                                           ((and (<=  right-key left-key)
                                                 (< right-key pos-key))
                                            right)  
                                           ((and (<= left-key right-key)  
                                                 (< left-key pos-key))
                                            left)  
                                           (T pos)))
                        ((equal size left) (cond
                                             ((< left-key pos-key)
                                              left)  
                                             (T pos))) 
                        (T pos))))
        (if (not (equal pos min-pos))
            (progn
              (heap-swap heap-id pos min-pos)
              (heap-sink heap-id min-pos)))))))




(defun heap-swim (heap-id p)
  (let ((heap-rep (gethash heap-id *heaps*)))
      (if (or (<= p 1) (> p (heap-size heap-rep))) p
      (let ((parent (floor p 2))
            (heap-array (heap-actual-heap heap-rep)))
        (if (< (first (aref heap-array p))
               (first (aref heap-array parent)))
            (progn
              (heap-swap heap-id p parent)
              (heap-swim heap-id parent)) p)))))


(defun heap-swap (heap-id pos1 pos2)
  (let* ((heap-rep (gethash heap-id *heaps*))
	 (temp (aref (heap-actual-heap heap-rep) pos1))
         (element1 (aref (heap-actual-heap heap-rep) pos1))
         (element2 (aref (heap-actual-heap heap-rep) pos2))) 

    (remhash (second element1) (heap-positions-hash-table heap-rep))
    (remhash (second element2) (heap-positions-hash-table heap-rep))
    (if (<= pos1 (heap-size heap-rep))
        (setf (gethash (second element2)
                       (heap-positions-hash-table heap-rep)) pos1))

    (if (<= pos2 (heap-size heap-rep))
        (setf (gethash (second element1)
                       (heap-positions-hash-table heap-rep)) pos2))

    (setf (aref (heap-actual-heap heap-rep) pos1)
          (aref (heap-actual-heap heap-rep) pos2))
    (setf (aref (heap-actual-heap heap-rep) pos2) temp)))


(defun heap-head (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (if (not (null heap-rep))
	(aref (heap-actual-heap (gethash heap-id *heaps*)) 1))))

(defun heap-full (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
     (equal (+ 1 (heap-size heap-rep)) (length (heap-actual-heap heap-rep)))))

(defun heap-insert (heap-id k v)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (if (not (null heap-rep))
        (let* ((new-pos (+ 1 (heap-size heap-rep)))
               (value-hash-table  (gethash v (heap-positions-hash-table heap-rep))))

	  (if (heap-full heap-id)
              (adjust-array (heap-actual-heap heap-rep) (+ 14 new-pos)))

	  (if (null value-hash-table)
              (progn
		(setf (heap-size heap-rep) new-pos)
		(setf (aref (heap-actual-heap heap-rep) new-pos) (list k v))
		(setf (gethash v (heap-positions-hash-table heap-rep)) new-pos)))
	  (heap-swim heap-id new-pos) T))))



(defun heap-extract (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
  (if (and (not (null heap-rep)) (heap-not-empty heap-id))
      (let ((head (heap-head heap-id)))
        (setf (heap-size heap-rep) (- (heap-size heap-rep) 1))
        (heap-swap heap-id 1 (+ 1 (heap-size heap-rep)))
        (heap-sink heap-id 1)
        head))))


(defun heap-modify-key (heap-id new-key old-key value) 
  (let* ((heap-rep (gethash heap-id *heaps*))
         (heap-pos (heap-position heap-id value old-key)))
    (if (not (null heap-pos))
        (progn
          (setf (aref (heap-actual-heap heap-rep) heap-pos) (list new-key value))
          (cond ((< old-key new-key)
                 (heap-sink heap-id heap-pos))
                ((> old-key new-key)
                 (heap-swim heap-id heap-pos)))
          T))))

(defun heap-print (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (if (not (null heap-rep))
	 (format t "SIZE: ~A, ~%ELEMENTS: ~D%"
            (heap-size heap-rep)
            (subseq (heap-actual-heap heap-rep) 1 (+ 1 (heap-size heap-rep)))))))



(defun mst-prim (graph-id source)
  (mst-init graph-id source mst-heap)
  (mst-prim-rec graph-id mst-heap)) 


(defun mst-prim-rec (graph-id heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
      (if (heap-not-empty (heap-id heap-rep))
      (let* ((head (heap-extract (heap-id heap-rep)))
             (head-id (third (second head)))
             (heap-array (heap-actual-heap heap-rep)))

        (setf (gethash (third (second head)) (gethash graph-id *vertex-keys*))
              (first head))

        (mapcar (lambda (arc)
                  (let ((pos
                          (heap-position heap-id
                                         (list 'vertex graph-id (fourth arc)))))

                    (if (and (not (null pos))
                             (> (first (aref heap-array pos)) (fifth arc)))
                        (progn
                          (heap-modify-key (heap-id heap-rep) (fifth arc)
                                           (first (aref heap-array pos))
                                           (second (aref heap-array pos)))

                          (setf (gethash (fourth arc)
                                         (gethash graph-id  *previous*))
                                head-id)))))
                (graph-vertex-neighbors graph-id head-id))
        (mst-prim-rec graph-id heap-id)))))



(defun mst-init (graph-id source heap-id)
  (let ((vertices (graph-vertices graph-id)))
    (heap-delete heap-id)
    (remhash graph-id *vertex-keys*)
    (remhash graph-id *previous*)
    (setf (gethash graph-id *previous*) (make-hash-table :test #'equal))
    (setf (gethash graph-id *vertex-keys*) (make-hash-table :test #'equal))
    (new-heap heap-id (list-length vertices))
    (mapcar (lambda (vertex)
              (if (equal (third vertex) source)
                  (progn
                    (heap-insert heap-id 0 vertex)
                    (setf (gethash (third vertex)
                                   (gethash graph-id *vertex-keys*)) 0)) 
                  (progn
                    (heap-insert heap-id MOST-POSITIVE-DOUBLE-FLOAT vertex)
                    (setf
                     (gethash (third vertex)  (gethash graph-id *vertex-keys*))
                     MOST-POSITIVE-DOUBLE-FLOAT))))
            vertices)) nil)


(defun mst-vertex-key (graph-id vertex-id)
  (if (not (null (gethash graph-id *vertex-keys*)))
      (gethash vertex-id (gethash graph-id  *vertex-keys*))))


(defun mst-previous (graph-id vertex-id)
  (if (not (null (gethash graph-id *previous*)))
      (gethash vertex-id (gethash graph-id *previous*))))


(defun mst-get (graph-id source)
  (if (is-vertex graph-id source)
      (progn
        (clrhash *visited*)
        (clrhash *graph-mst-temp*)
        (build-mst-graph graph-id)
        (mst-get-rec graph-id source))))

(defun mst-get-rec (graph-id source)
  (set-visited source)
  (mst-get-list graph-id
                (stable-sort
                 (stable-sort
                  (free-arcs graph-id source) #'atom< :key 'fourth)
                 '< :key 'fifth)))


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
  (new-arc 'm '2 "2"))


(defun build-mst-graph (graph-id)
  (maphash (lambda (k v)
	   (setf (gethash (third v) *graph-mst-temp*)
                 (make-hash-table :test #'equal)))
	   *vertices*)
  (maphash (lambda (k v)
	     (let ((previous (mst-previous graph-id (third v))))
	       (if (not (null previous))
		   (progn
		     
		     (setf (gethash (third v) (gethash previous
						       *graph-mst-temp*))
			   (mst-vertex-key graph-id (third k)))

		     (setf (gethash previous (gethash (third v)
						      *graph-mst-temp*))
			   (mst-vertex-key graph-id (third k)))))))
  *vertices*))

(defun mst-get-list (graph-id arcs)
  (if (not (null arcs))
      (cons (car arcs)
            (append (mst-get-rec graph-id (fourth (car arcs)))
                    (mst-get-list graph-id (cdr arcs))))))

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


;;;; atom< is needed for treating numbers as strings during the arcs sorting
;;;; in mst-get
(defun atom< (a b)
  (let ((string-a (if (stringp a)
                      a
                      (write-to-string a)))
        (string-b (if (stringp b)
                      b
                      (write-to-string b))))
    (if (string< string-a string-b)
        0
        NIL))) 

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
  (new-arc 'g 'b 'a 4)
  (new-arc 'g 'c 'b 8)
  (new-arc 'g 'c 'd 7)
  (new-arc 'g 'd 'e 9)
  (new-arc 'g 'e 'f 10)
  (new-arc 'g 'f 'g 2)
  (new-arc 'g 'g 'h 1)
  (new-arc 'g 'a 'h 8)
  (new-arc 'g 'g 'i 6)
  (new-arc 'g 'i 'h 7)
  (new-arc 'g 'h 'b 11)()
  (new-arc 'g 'd 'f 14)
  (new-arc 'g 'c 'f 4)
  (new-arc 'g 'c 'i 2))

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


(defun prim-and-get (g v1 v2)
  (mst-prim g v1)
  (mst-get g v2) nil)




(defun somma-giusta (lista)
  (if (not (null lista))
      (+ (somma-giusta (cdr lista)) (fifth (car lista)))
      0))







(defun somma-archi (graph-id)
  (let ((somma 0))
    (maphash (lambda (k v)
               (setf somma (+ somma v)))
             (gethash graph-id *vertex-keys*))
    somma))


(defun print-hash (hash-table)
  (maphash (lambda (k v) (print (list k v))) hash-table))

(load "/Users/gabrielecanesi/Downloads/lisptest/primkiller_10k.LISP")

(defun lam (x y)
  ((lambda (a1 a2)
     (+ a1 a2))
   (+ 1 x)
   (+ 1 y)))
