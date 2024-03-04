; -*- Mode: Lisp -*-
; mst.lisp

;; 844526 Kolyszko Matteo
;; 844529 Loddo Luca
;; 845374 Arizzi Sara

;; definizione parametri

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

(defparameter *lista-heap* nil)
(defparameter *lista* nil)
(defparameter *testa* nil) 
(defparameter *lista-child* nil)
(defparameter *lista-child-spec* nil)
(defparameter *preorder-mst* nil)

;; implementazione minheap

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-id (heap-rep)
  (second heap-rep))

(defun heap-size (heap-rep)
  (third heap-rep))

(defun heap-actual-heap (heap-rep)
  (fourth heap-rep))

(defun is-heap (heap-id)
  (or 
   (gethash heap-id *heaps*)
   nil))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))

(defun heap-empty (heap-id)
  (= 0 (heap-size (gethash heap-id *heaps*))))

(defun heap-not-empty (heap-id)
	(not (heap-empty heap-id)))

(defun heap-head (heap-id)
	(if (heap-not-empty heap-id)
          	(get-value heap-id 0)
	nil))

(defun heap-insert (heap-id key value)
  (if (is-heap heap-id)
      (cond 
       ((= (heap-size (gethash heap-id *heaps*))
           (array-total-size
            (heap-actual-heap (gethash heap-id *heaps*))))
        nil
        )
       (T (and 
           (parental-control heap-id 
                             (heap-size 
                              (gethash heap-id *heaps*)) 
                             key 
                             value)
           (change-size heap-id 1))
          T))))

(defun parental-control (heap-id i key value)
  (if (and 
       (< 0 i)
       (> (car (get-value heap-id (parent i))) key))
      (and (set-value heap-id i (aref 
         (heap-actual-heap (gethash heap-id *heaps*)) 
         (parent i)))
       (parental-control heap-id (parent i) key value))
    (set-value heap-id i (list key value))))

(defun parent (i)
  (floor (/ (- i 1) 2)))

(defun change-size (heap-id amount)
  (setf 
   (gethash heap-id *heaps*)
   (list 'heap heap-id
         (+ amount 
            (heap-size (gethash heap-id *heaps*)))
         (heap-actual-heap (gethash heap-id *heaps*)))))

(defun get-value (heap-id i)
  (aref (heap-actual-heap (gethash heap-id *heaps*))
        i))

(defun set-value (heap-id i value)
  (setf (aref 
         (heap-actual-heap (gethash heap-id *heaps*)) ;
         i)
        value))

(defun heap-extract (heap-id)
  (if (is-heap heap-id)
      (if (heap-not-empty heap-id)
          (and 
           (setq *testa* (heap-head heap-id))
           (if (= 1 (heap-size (gethash heap-id *heaps*)))
               (and 
                (not (set-value heap-id 0 nil))
                (change-size heap-id (- 1))
                *testa*)
             (and 
              (change-size heap-id (- 1))
              (set-value heap-id 0 
                         (get-value 
                          heap-id
                          (heap-size 
                           (gethash heap-id *heaps*))))
              (not 
               (set-value heap-id 
                          (heap-size (gethash heap-id *heaps*)) 
                          nil))
              (heapify heap-id 0 
                       (first (heap-head heap-id))
                       (second (heap-head heap-id)))
              *testa*)))
        nil)
    nil))

(defun heapify (heap-id i key value)
  (cond
   ((< (heap-size (gethash heap-id *heaps*)) (+ 1 (+ 1 (* 2 i))))
    T)
   ((= (heap-size (gethash heap-id *heaps*)) (+ 1 (+ 1 (* 2 i))))
    (if (< (car (get-value heap-id (+ 1 (* 2 i))))
           (car (get-value heap-id i)))
        (and 
         (set-value heap-id i (get-value heap-id (+ 1 (* 2 i))))
         (set-value heap-id (+ 1 (* 2 i)) (list key value)))
      (set-value heap-id i (list key value))))
   (T (if (< (car (get-value heap-id (+ 1 (* 2 i))))
             (car (get-value heap-id (+ 2 (* 2 i)))))
          (if (< (car (get-value heap-id (+ 1 (* 2 i))))
                 (car (get-value heap-id i)))
            (and 
             (set-value heap-id i (get-value heap-id (+ 1 (* 2 i))))
             (set-value heap-id (+ 1 (* 2 i)) (list key value))
             (heapify heap-id (+ 1 (* 2 i))
                      (first (get-value heap-id (+ 1 (* 2 i))))
                      (second (get-value heap-id (+ 1 (* 2 i))))))
            (set-value heap-id i (list key value)))
        (if (< (car (get-value heap-id (+ 2 (* 2 i))))
               (car (get-value heap-id i)))
            (and 
             (set-value heap-id i (get-value heap-id (+ 2 (* 2 i))))
             (set-value heap-id (+ 2 (* 2 i)) (list key value))
             (heapify heap-id (+ 2 (* 2 i))
                      (first (get-value heap-id (+ 2 (* 2 i))))
                      (second (get-value heap-id (+ 2 (* 2 i))))))
          (set-value heap-id i (list key value)))))))

(defun heap-modify-key (heap-id new-key old-key value)
  (if (is-heap heap-id)
      (if (heap-not-empty heap-id)
          (progn
           (set-value heap-id
                      (get-index-key heap-id 0 old-key value)
                      (list new-key value))
           (parental-control 
            heap-id        
            (get-index-key heap-id 0 new-key value) new-key value)
           (heapify heap-id 
                    (get-index-key heap-id 0 new-key value)
		    new-key value)
           T)
        nil)
    nil))

(defun get-index-key (heap-id i key value)
  (if (= i (array-total-size 
            (heap-actual-heap (gethash heap-id *heaps*))))
      nil
    (if (and (= (first (get-value heap-id i)) key)
             (equal (second (get-value heap-id i))  value))
        i
      (get-index-key heap-id (+ i 1) key value))))

(defun get-key (heap-id i value)
  (if (= i (array-total-size
            (heap-actual-heap (gethash heap-id *heaps*))))
      nil
    (if (equal (second (get-value heap-id i)) value)
        (first (get-value heap-id i))
      (get-key heap-id (+ i 1) value))))

(defun heap-print (heap-id)
  (if (is-heap heap-id)
      (array-print heap-id 0)
    nil))

(defun array-print (heap-id i)
  (if (= i (heap-size (gethash heap-id *heaps*)))
      T
    (and (print (get-value heap-id i))
         (array-print heap-id (+ i 1)))))

;; implementazione interfaccia per la manipolazione di grafi

(defun is-graph (graph-id)
  (or 
   (gethash graph-id *graphs*)
   nil))

(defun new-graph (graph-id)
  (or 
   (is-graph graph-id)
   (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
  (if (is-graph graph-id)
      (and 
       (remhash graph-id *graphs*)
       (remhash graph-id *vertex-keys*)
       (remhash graph-id *previous*)
       (maphash #' (lambda (k v)
                     (declare (ignore v))
                      (if (equal (second k) graph-id) 
                          (remhash k *vertices*)))
                 *vertices*)
       (maphash #' (lambda (k v)
                     (declare (ignore v))
                     (if (equal (second k) graph-id)
                         (remhash k *arcs*))) 
                *arcs*))
    nil))

(defun is-vertex (graph-id vertex-id)
  (if (is-graph graph-id)
      (or (gethash
           (list 'vertex graph-id vertex-id) *vertices*) nil)
    nil))

(defun new-vertex (graph-id vertex-id)
  (if (is-graph graph-id)
      (or (is-vertex graph-id vertex-id)
       (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
             (list 'vertex graph-id vertex-id)))
    nil))

(defun graph-vertices (graph-id)
  (if (is-graph graph-id)
      (or (setq *lista* nil)
       (maphash #' (lambda (k v)
          (and (equal (second k) graph-id)
           (setq *lista* (append (list v) *lista*))))
                *vertices*)
       *lista*)
    nil))

(defun is-arc (graph-id vertex-1-id vertex-2-id)
  (if (and 
       (is-graph graph-id)
       (is-vertex graph-id vertex-1-id)
       (is-vertex graph-id vertex-2-id))
      (or 
       (gethash 
        (list 'arc graph-id vertex-1-id vertex-2-id)
        *arcs*)
       (gethash
        (list 'arc graph-id vertex-2-id vertex-1-id)
        *arcs*)
       nil)
    nil))

(defun new-arc (graph-id vertex-1-id vertex-2-id &optional (weight 1))
  (if (and 
       (is-graph graph-id)
       (is-vertex graph-id vertex-1-id) 
       (is-vertex graph-id vertex-2-id)
       (not (equal vertex-1-id vertex-2-id))) 
      (cond ((not (equal 
                   nil 
                   (or (gethash
                        (list 'arc graph-id vertex-1-id vertex-2-id )
                        *arcs*) nil)))
             (setf 
              (gethash (list 'arc graph-id vertex-1-id vertex-2-id)
                       *arcs*)
              (list 'arc graph-id vertex-1-id vertex-2-id weight)))
            ((not (equal 
                   nil 
                   (or (gethash
                        (list 'arc graph-id vertex-2-id vertex-1-id )
                        *arcs*) nil)))
             (setf 
              (gethash (list 'arc graph-id vertex-2-id vertex-1-id)
                       *arcs*)
              (list 'arc graph-id vertex-1-id vertex-2-id weight)))
            ((equal nil (and 
                        (gethash
                        (list 'arc graph-id vertex-1-id vertex-2-id )
                        *arcs*)
                        (gethash
                        (list 'arc graph-id vertex-2-id vertex-1-id )
                        *arcs*)))
             (setf 
              (gethash (list 'arc graph-id vertex-1-id vertex-2-id)
                       *arcs*)
              (list 'arc graph-id vertex-1-id vertex-2-id weight))))
    nil))

(defun graph-arcs (graph-id)
 (if (is-graph graph-id)
     (or 
      (setq *lista* nil)
      (maphash #' 
               (lambda (k v)
                 (and 
                  (equal (second k) graph-id)
                  (setq *lista* (append (list v) *lista*)))) 
               *arcs*)
      *lista*)
   nil))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (if (and 
       (is-graph graph-id)
       (is-vertex graph-id  vertex-id))
      (or 
       (setq  *lista* nil)
       (maphash #' (lambda (k v)
         (cond 
          ((equal (third k) vertex-id)
           (setq *lista* (append *lista* (list v))))
          ((equal (fourth k) vertex-id)
           (setq *lista* (append *lista* (list v)))) 
          (T T)))
                *arcs*)
       *lista*)
    nil))

(defun graph-vertex-adjacent (graph-id vertex-id)
  (if (and 
       (is-graph graph-id)
       (is-vertex graph-id  vertex-id))
      (or 
       (setq  *lista* nil)
       (maphash #' (lambda (k v)
                     (cond 
                      ((equal (third k) vertex-id)
                       (setq 
                        *lista* 
                        (append *lista* 
                                (list (list  
                                       'vertex
                                       (second v)
                                       (fourth v))))))
                      ((equal (fourth k) vertex-id)
                       (setq 
                        *lista* 
                        (append *lista* 
                                (list (list
                                       'vertex
                                       (second v)
                                       (fourth v)))))) (T T)))
                *arcs*)
       *lista*)
    nil))

(defun graph-print (graph-id)
  (if (is-graph graph-id)
      (or 
       (maphash #'
        (lambda (k v)
          (and (equal (second k) graph-id) 
               (print v)))
                *vertices*)
       (maphash #' 
        (lambda (k v)
          (and (equal (second k) graph-id)
               (print v)))
                *arcs*))))

;; implementazione algoritmo di Prim

(defun member-2 (x xs)
  (cond 
   ((null xs) nil)
   ((not (equal x (first xs))) (member-2 x (rest xs)))
   ((equal x (first xs)) T)))

(defun mst-prim (graph-id source)
  (reset graph-id)
  (if (and (is-graph graph-id)
           (is-vertex graph-id source))
      (and
       (new-heap graph-id (list-length (graph-vertices graph-id))) 
       (initialize-prim graph-id source (graph-vertices graph-id))
       (extract-all-heap graph-id (graph-vertices graph-id))))
  nil)

(defun reset (graph-id)
  (setq *lista-heap* nil)
  (clrhash *previous*)
  (clrhash *vertex-keys*)
  (heap-delete graph-id))

(defun extract-all-heap (graph-id vertex-rep-list)
  (if (heap-not-empty graph-id)
      (and
       (setq 
        *lista-heap* 
        (append 
         (list (second (heap-head graph-id))) 
         *lista-heap*))
       (prim 
        graph-id 
        *lista-heap* 
        (second (heap-head graph-id)) 
        (graph-vertex-neighbors
         graph-id 
         (second (heap-extract graph-id))))
       (extract-all-heap graph-id (rest vertex-rep-list)))
    T))
    
(defun prim (graph-id *lista-heap* start-vertex neighbors)
  (if (null neighbors) T 
    (if (equal start-vertex (third (first neighbors)))
	(if (not (member-2 (fourth (first neighbors)) *lista-heap*))
               (if (< (fifth (first neighbors))
                      (mst-vertex-key graph-id
				      (fourth (first neighbors))))
                   (and (update-prim graph-id
                                     (fourth (first neighbors))
                                     start-vertex
                                     (fifth (first neighbors)))
                        (prim graph-id *lista-heap* 
                              start-vertex (rest neighbors)))
		 (prim graph-id *lista-heap* start-vertex
		       (rest neighbors)))
          (prim graph-id *lista-heap* start-vertex (rest neighbors)))
      (if (not (member-2 (third (first neighbors)) *lista-heap*))
           (if (< (fifth (first neighbors))
                  (mst-vertex-key graph-id (third (first neighbors))))
               (and (update-prim graph-id
                                 (third (first neighbors))
                                 start-vertex
                                 (fifth (first neighbors)))
                    (prim graph-id *lista-heap* 
                          start-vertex (rest neighbors)))
           (prim graph-id *lista-heap* start-vertex (rest neighbors)))
        (prim graph-id *lista-heap* start-vertex (rest neighbors))))))

(defun update-prim (graph-id v u w)
  (and 
   (cond ((gethash
	   (list 'vertex-key graph-id v MOST-POSITIVE-DOUBLE-FLOAT)
	   *vertex-keys*)
          (setf (gethash
		 (list 'vertex-key graph-id v
		       MOST-POSITIVE-DOUBLE-FLOAT) 
              *vertex-keys*)
             (list 'vertex-key graph-id v w)))
         ((gethash (list 'vertex-key graph-id v w) *vertex-keys*)
          (setf (gethash
		 (list 'vertex-key graph-id v w) *vertex-keys*)
             (list 'vertex-key graph-id v w))))
   (heap-modify-key graph-id w (get-key graph-id 0 v) v)
   (cond ((gethash (list 'vertex-previous graph-id v nil) *previous*)
          (setf (gethash
		 (list 'vertex-previous graph-id v nil) *previous*)
             (list 'vertex-previous graph-id v u)))
       ((gethash (list 'vertex-previous graph-id v u) *previous*)
        (setf
	 (gethash (list 'vertex-previous graph-id v u) *previous*)
             (list 'vertex-previous graph-id v u))))))

(defun initialize-prim (graph-id source vertex-rep-list)
  (cond  
   ((null vertex-rep-list) T)
   ((equal source (third (first vertex-rep-list)))
    (and 
     (heap-insert graph-id 0 source)
     (setf
      (gethash (list 'vertex-key graph-id source 0) *vertex-keys*)
      (list 'vertex-key graph-id source 0) )
     (setf
      (gethash (list 'vertex-previous graph-id source 'nil)
	       *previous*)
      (list 'vertex-previous graph-id source 'nil))
     (initialize-prim graph-id source (cdr vertex-rep-list))))
   (T (and 
       (heap-insert graph-id MOST-POSITIVE-DOUBLE-FLOAT 
                        (third (first vertex-rep-list)))
       (setf
             (gethash 
              (list 'vertex-key graph-id 
                    (third (first vertex-rep-list)) 
                    MOST-POSITIVE-DOUBLE-FLOAT) 
              *vertex-keys*)
            (list 'vertex-key graph-id 
                    (third (first vertex-rep-list)) 
                    MOST-POSITIVE-DOUBLE-FLOAT) )
       (setf 
           (gethash 
            (list 'vertex-previous graph-id 
                  (third (first vertex-rep-list)) 
                  'nil) 
            *previous*) 
          (list 'vertex-previous graph-id 
                    (third (first vertex-rep-list)) 'nil) )
     (initialize-prim graph-id source (cdr vertex-rep-list)))))
  T)

(defun mst-vertex-key (graph-id vertex-id)
  (let ((peso 0))
    (if (and (is-graph graph-id)
             (is-vertex graph-id vertex-id))
        (progn (maphash  #'
                         (lambda (k v)
                           (and (equal (second k) graph-id)
                                (equal (third k) vertex-id)
                                (setf peso (fourth v))))
                         *vertex-keys*)
          (+ peso 0)))))

(defun mst-previous (graph-id vertex-id)
  (let ((parent nil)) 
    (if (and (is-graph graph-id)
             (is-vertex graph-id vertex-id))
        (progn (maphash  #'
                     (lambda (k v)
                       (and (equal (second k) graph-id)
                            (equal (third v) vertex-id)
                            (setf parent (fourth v))))
                         *previous*)
          (append nil parent)))))

(defun mst-get (graph-id source)
  (setq *lista-child* nil)
  (setq *preorder-mst* nil)
  (if (and (is-graph graph-id)
           (is-vertex graph-id source))
      (empty-list graph-id (find-child graph-id source))))

(defun find-child (graph-id parent)
  (setq *lista-child-spec* nil)
  (maphash #' 
           (lambda (k v)
             (cond 
              ((and (equal (second k) graph-id)
                    (equal (fourth v) parent))
               (setq *lista-child-spec* 
                     (append (list
                              (list
                               parent
                               (third v)
                               (mst-vertex-key graph-id
					       (third v))))
                             *lista-child-spec*)))))
           *previous*)
  (setq *lista-child* (append
                       (sort (copy-list *lista-child-spec*) 
                             (and-then 'coefficient< 'variable<))
                       *lista-child*)))

(defun empty-list (graph-id lista-child)
  (if (null lista-child)
      (append (reverse *preorder-mst*) nil)
    (if (not (has-child graph-id (second (first lista-child))))
        (and (add graph-id (first (first lista-child)) 
                  (second (first lista-child)))
             (remove-first lista-child)
             (empty-list graph-id *lista-child*))
      (and (add graph-id (first (first lista-child)) 
                (second (first lista-child)))
           (remove-first lista-child)
           (find-child graph-id (second (first lista-child)))
           (empty-list graph-id *lista-child*))))
  (append (reverse *preorder-mst*) nil))

(defun has-child (graph-id vertex-id)
  (let ((cont 0)) 
    (progn 
      (maphash #' 
               (lambda (k v)
                 (cond 
                  ((and (equal (second k) graph-id)
                        (equal (fourth v) vertex-id)
                        (setf cont (+ 1 cont))))))
               *previous*)
      (if (= 0 cont)
          nil
        T))))

(defun add (graph-id v1 v2)
  (push (is-arc graph-id v1 v2) *preorder-mst*)
  T)

(defun remove-first (lista-child)
  (or (setq *lista-child* (rest lista-child))
      T))

(defun and-then (peso lettera)
  (lambda (x y)
    (cond
      ((funcall peso x y) t)
      ((funcall peso y x) nil)
      (t (funcall lettera x y)))))

(defun term-coefficient (term)
  (third term))

(defun coefficient< (term1 term2)
  (< (term-coefficient term1)
     (term-coefficient term2)))

(defun term-variable (term)
  (second term))

(defun variable< (term1 term2)
  (string< (term-variable term1)
           (term-variable term2)))

; End of file -- mst.lisp
