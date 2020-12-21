3(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *graph-mst-temp* (make-hash-table :test #'equal))

(defstruct heap
  id
  size
  actual-heap
  positions-hash-table)


(ql:quickload "cl-csv")

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (if (null (is-graph graph-id))
      (progn
	(setf (gethash graph-id *arcs*) (make-hash-table :test #'equal))
	(setf (gethash graph-id *graphs*) graph-id))))

(defun is-vertex (graph-id vertex-id)
  (gethash (list 'vertex graph-id vertex-id) *vertices*))

(defun new-vertex (graph-id vertex-id)
  (if (null (gethash (list 'vertex graph-id vertex-id) *vertices*))
      (progn
	(setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
              (list 'vertex graph-id vertex-id))
	(setf (gethash vertex-id (gethash graph-id *arcs*))
	      (make-hash-table :test #'equal)))))


(defun is-arc (graph-id v1-id v2-id)
   (not (null (gethash (list 'arc graph-id v1-id v2-id) *arcs*))))


(defun new-arc (graph-id v1-id v2-id &optional (weight 1))
  (if (and (is-graph graph-id) (>= weight 0))
      (progn
	(new-vertex graph-id v2-id)
	(new-vertex graph-id v1-id)
	(setf (gethash v2-id (gethash v1-id (gethash graph-id *arcs*)))
	      weight)
	(setf (gethash v1-id (gethash v2-id (gethash graph-id *arcs*)))
	      weight))))


(defun graph-vertices (graph-id)
  (let ((vertex-list))
    (maphash (lambda (k v)
               (if (equal (second k) graph-id)
		   (push  (list 'vertex graph-id
                                (third k)) vertex-list)
		   ))
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
  (let* ((list)
	 (neighbors (progn
		      (maphash
		       (lambda (k v)
			 (push (list
				'arc graph-id vertex-id k v)
			       list))
		       (gethash vertex-id (gethash graph-id *arcs*)))
		      list)))
    neighbors))


;(defun graph-vertex-adjacent (graph-id vertex-id)
; (let ((va (loop for k being the hash-keys of *arcs*
;		      collect (if (and
;			   (equal (second k) graph-id)
;				   (equal (third k) vertex-id))
;				  (list 'vertex graph-id (fourth
;							  k)))))
;      (v (loop for k being the hash-keys of *arcs*
;		      collect (if (and
;			   (equal (second k) graph-id)
;				   (equal (third k) vertex-id))
;				  (list 'vertex graph-id (fourth
;						  k)))))) 
;  (remove-if (lambda (x) (null x)) hash-list)))



(defun graph-print (graph-id)
  (if (is-graph graph-id)
      (progn
	(print graph-id)
	(maphash (lambda (k v)
		   (if (equal (second k) graph-id)
		      (print v) )
		   ) *vertices*)
	(maphash (lambda (k v)
		   (maphash (lambda (k1 v1)
			      (print (list 'arc k k1 v1))) v))
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



(defun heap-position (heap-rep value &optional (key nil))
  (let ((pos))
	  (if (null key)
	      (setf pos (gethash value (heap-positions-hash-table heap-rep)))
	      (if (equal (first (aref (heap-actual-heap heap-rep)
				      (gethash value (heap-positions-hash-table heap-rep)))) key)
		  (setf pos (gethash value (heap-positions-hash-table heap-rep)))))
    pos))

(defun heap-sink (heap-rep pos)
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
          (heap-swap heap-rep pos min-pos)
          (heap-sink heap-rep min-pos))))))




(defun heap-swim (heap-rep p)
  (if (or (<= p 1) (> p (heap-size heap-rep))) p
      (let ((parent (floor p 2))
            (heap-array (heap-actual-heap heap-rep)))
	(if (< (first (aref heap-array p))
	       (first (aref heap-array parent)))
            (progn
              (heap-swap heap-rep p parent)
              (heap-swim heap-rep parent)) p))))


(defun heap-swap (heap-rep pos1 pos2)
  (let* ((temp (aref (heap-actual-heap heap-rep) pos1))
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
  (aref (heap-actual-heap (gethash heap-id *heaps*)) 1))

(defun heap-full (heap-rep)
  (equal (+ 1 (heap-size heap-rep)) (length (heap-actual-heap heap-rep))))

(defun heap-insert (heap-id k v)
  (let* ((heap-rep (gethash heap-id *heaps*))
         (new-pos (+ 1 (heap-size heap-rep)))
	 (value-hash-table  (gethash v (heap-positions-hash-table heap-rep))))
    
    (if (heap-full heap-rep)
	(adjust-array (heap-actual-heap heap-rep) (+ 15 new-pos)))
    
    (if (null value-hash-table)
	(progn
	  (setf (heap-size heap-rep) new-pos)
	  (setf (aref (heap-actual-heap heap-rep) new-pos) (list k v))
	  (setf (gethash v (heap-positions-hash-table heap-rep)) new-pos)))
  (heap-swim heap-rep new-pos)))



(defun heap-extract (heap-id)
  (if (heap-not-empty heap-id)
      (let* ((heap-rep (gethash heap-id *heaps*))
             (head (heap-head heap-id)))
	(setf (heap-size heap-rep) (- (heap-size heap-rep) 1))
	(heap-swap heap-rep 1 (+ 1 (heap-size heap-rep)))
	(heap-sink heap-rep 1)
	head)))


(defun heap-modify-key (heap-id new-key old-key value) 
  (let* ((heap-rep (gethash heap-id *heaps*))
	 (heap-pos (heap-position heap-rep value old-key)))
    
    (setf (aref (heap-actual-heap heap-rep) heap-pos) (list new-key value))
    
    
    (cond ((< old-key new-key)
           (heap-sink heap-rep heap-pos))
          ((> old-key new-key)
           (heap-swim heap-rep heap-pos)))))

(defun heap-print (heap-id)
  (format t "DIMENSIONE: ~A, \NELEMENTI: ~D%"
          (heap-size heap-id) (heap-actual-heap heap-id)))



(defun mst-prim (graph-id source)
  (mst-init graph-id source 'mst-heap)
  (mst-prim-rec graph-id (gethash 'mst-heap *heaps*))) 


(defun mst-prim-rec (graph-id heap-rep)
  (if (heap-not-empty (heap-id heap-rep))
      (let* ((head (heap-extract (heap-id heap-rep)))
	     (head-id (third (second head)))
	     (heap-array (heap-actual-heap heap-rep)))

	(setf (gethash (third (second head)) (gethash graph-id *vertex-keys*))
	      (first head))

;	(mapcar (lambda (arc)
;		   (let ((pos
;			   (heap-position heap-rep (list 'vertex graph-id (fourth arc)))))
;		     (if (and (not (null pos)) (> (first (aref heap-array pos)) (fifth arc)))
;			     (progn
;			       (heap-modify-key (heap-id heap-rep) (fifth arc)
;						(first (aref heap-array pos))
;						(second (aref heap-array pos)))
;			       
;			       (setf (gethash (list 'vertex graph-id (fourth arc))
;					      (gethash graph-id  *previous*))
;				     (list 'vertex graph-id head-id))))))
;		 (graph-vertex-neighbors graph-id head-id))
	
	
	(maphash (lambda (k v)
		   (let ((pos
			   (heap-position heap-rep (list 'vertex graph-id k))))
			 (if (and (not (null pos)) (> (first (aref heap-array pos)) v))
			     (progn
			       (heap-modify-key (heap-id heap-rep) v
						(first (aref heap-array pos))
						(second (aref heap-array pos)))
			       
			       (setf (gethash (list 'vertex graph-id k)
					     (gethash graph-id  *previous*))
				     (list 'vertex graph-id head-id))))))
		 (gethash (third (second head)) (gethash graph-id *arcs*)))
	
      (mst-prim-rec graph-id heap-rep))))



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
  (gethash vertex-id (gethash graph-id  *vertex-keys*)))


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
		  (free-arcs graph-id source) 'string< :key 'fourth)
		 '< :key 'fifth)))


(defun build-mst-graph (graph-id)
  (maphash (lambda (k v)
	     (if (null (gethash (third v) *graph-mst-temp*))
		   (setf (gethash (third v) *graph-mst-temp*)
			 (make-hash-table :test #'equal)))
	     
	     (if (null (gethash (third k) *graph-mst-temp*))
	      (setf (gethash (third k) *graph-mst-temp*)
			 (make-hash-table :test #'equal)))
	     
	   
	     (setf (gethash (third v) (gethash (third k) *graph-mst-temp*))
		   (mst-vertex-key graph-id (third k)))
	     
	     (setf (gethash (third k) (gethash (third v) *graph-mst-temp*))
		   (mst-vertex-key graph-id (third k))))
	   (gethash graph-id *previous*)))

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



(require "cl-csv")


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

(defun heap-test ()
  (new-heap 'h 80)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5)
  (heap-insert 'h 67 3)
  (heap-insert 'h 99 2)
  (heap-insert 'h 2 66)
  (heap-insert 'h 5 5))


(defun kill ()
  (new-graph 'kill)
  (cl-csv:read-csv #P"~/primkiller_definitivo.csv"
		    :separator #\Tab
		    :map-fn #' (lambda (row) ;(print row)
				 (new-arc 'kill
					  (first row)
					  (second row)
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
	       (setf somma (+ somma v))) (gethash graph-id *vertex-keys*))
    somma))


(defun print-hash (hash-table)
  (maphash (lambda (k v) (print (list k v))) hash-table))
