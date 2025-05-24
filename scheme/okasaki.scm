#!/usr/bin/env guile
;; list.scm - Persistent List implementation
(define-module (okasaki list)
  #:export (empty? cons-list head tail))
;; Basic list operations
(define empty? null?)
(define cons-list cons)
(define head car)
(define tail cdr)
;; Example usage
(define example-list (cons-list 1 (cons-list 2 (cons-list 3 '()))))
(display "List: ")
(display example-list)
(newline)
(display "Head: ")
(display (head example-list))
(newline)
(display "Tail: ")
(display (tail example-list))
(newline)
(display "Is empty? ")
(display (empty? example-list))
(newline)

;; stack.scm - Persistent Stack implementation

(define-module (okasaki stack)
  #:export (empty-stack empty-stack? push pop top))

;; Stack is implemented as a list
(define empty-stack '())

(define (empty-stack? stack)
  (null? stack))

(define (push item stack)
  (cons item stack))

(define (top stack)
  (if (empty-stack? stack)
      (error "Cannot get top of empty stack")
      (car stack)))

(define (pop stack)
  (if (empty-stack? stack)
      (error "Cannot pop empty stack")
      (cdr stack)))

;; Example usage
(display "\nStack Operations:\n")
(define my-stack empty-stack)
(set! my-stack (push 3 my-stack))
(set! my-stack (push 2 my-stack))
(set! my-stack (push 1 my-stack))
(display "Stack: ")
(display my-stack)
(newline)
(display "Top: ")
(display (top my-stack))
(newline)
(display "After pop: ")
(display (pop my-stack))
(newline)

;; queue.scm - Persistent Queue implementation using two lists

(define-module (okasaki queue)
  #:export (empty-queue empty-queue? enqueue dequeue front))

;; Queue is represented as a pair of lists (front-list . rear-list)
;; Items are enqueued to the rear list and dequeued from the front list
;; When front list is empty, the rear list is reversed and becomes the new front list

(define empty-queue (cons '() '()))

(define (empty-queue? queue)
  (and (null? (car queue)) (null? (cdr queue))))

(define (check-queue queue)
  (if (null? (car queue))
      (if (null? (cdr queue))
          empty-queue
          (cons (reverse (cdr queue)) '()))
      queue))

(define (enqueue item queue)
  (check-queue (cons (car queue) (cons item (cdr queue)))))

(define (front queue)
  (if (empty-queue? queue)
      (error "Cannot get front of empty queue")
      (car (car queue))))

(define (dequeue queue)
  (if (empty-queue? queue)
      (error "Cannot dequeue empty queue")
      (check-queue (cons (cdr (car queue)) (cdr queue)))))

;; Example usage
(display "\nQueue Operations:\n")
(define my-queue empty-queue)
(set! my-queue (enqueue 1 my-queue))
(set! my-queue (enqueue 2 my-queue))
(set! my-queue (enqueue 3 my-queue))
(display "Queue: ")
(display my-queue)
(newline)
(display "Front: ")
(display (front my-queue))
(newline)
(display "After dequeue: ")
(set! my-queue (dequeue my-queue))
(display my-queue)
(newline)
(display "New front: ")
(display (front my-queue))
(newline)

;; binomial-heap.scm - Persistent Binomial Heap implementation

(define-module (okasaki binomial-heap)
  #:export (empty-heap empty-heap? insert find-min delete-min merge))

;; A binomial tree is represented as (rank value children)
;; A binomial heap is a list of binomial trees ordered by increasing rank

(define empty-heap '())

(define (empty-heap? heap)
  (null? heap))

(define (rank tree)
  (car tree))

(define (root-value tree)
  (cadr tree))

(define (children tree)
  (caddr tree))

(define (make-tree rank value children)
  (list rank value children))

(define (link t1 t2)
  (if (< (root-value t1) (root-value t2))
      (make-tree (+ (rank t1) 1) 
                 (root-value t1) 
                 (cons t2 (children t1)))
      (make-tree (+ (rank t2) 1) 
                 (root-value t2) 
                 (cons t1 (children t2)))))

(define (insert-tree tree heap)
  (if (null? heap)
      (list tree)
      (let ((t (car heap)) (ts (cdr heap)))
        (if (< (rank tree) (rank t))
            (cons tree heap)
            (insert-tree (link tree t) ts)))))

(define (insert x heap)
  (insert-tree (make-tree 0 x '()) heap))

(define (merge heap1 heap2)
  (cond ((null? heap1) heap2)
        ((null? heap2) heap1)
        (else
         (let ((t1 (car heap1)) (ts1 (cdr heap1))
               (t2 (car heap2)) (ts2 (cdr heap2)))
           (cond ((< (rank t1) (rank t2))
                  (cons t1 (merge ts1 heap2)))
                 ((< (rank t2) (rank t1))
                  (cons t2 (merge heap1 ts2)))
                 (else
                  (insert-tree (link t1 t2) (merge ts1 ts2))))))))

(define (find-min-tree heap)
  (if (null? (cdr heap))
      (car heap)
      (let ((t (car heap))
            (t' (find-min-tree (cdr heap))))
        (if (< (root-value t) (root-value t'))
            t
            t'))))

(define (find-min heap)
  (if (empty-heap? heap)
      (error "Cannot find min of empty heap")
      (root-value (find-min-tree heap))))

(define (remove-min-tree heap)
  (if (null? (cdr heap))
      (list (car heap) '())
      (let* ((rest-result (remove-min-tree (cdr heap)))
             (t' (car rest-result))
             (ts' (cadr rest-result))
             (t (car heap)))
        (if (< (root-value t) (root-value t'))
            (list t (cdr heap))
            (list t' (cons t ts'))))))

(define (delete-min heap)
  (if (empty-heap? heap)
      (error "Cannot delete min from empty heap")
      (let* ((result (remove-min-tree heap))
             (min-tree (car result))
             (rest-heap (cadr result))
             (rev-children (reverse (children min-tree))))
        (merge rev-children rest-heap))))

;; Example usage
(display "\nBinomial Heap Operations:\n")
(define my-heap empty-heap)
(set! my-heap (insert 5 my-heap))
(set! my-heap (insert 3 my-heap))
(set! my-heap (insert 7 my-heap))
(set! my-heap (insert 1 my-heap))
(display "Min value: ")
(display (find-min my-heap))
(newline)
(set! my-heap (delete-min my-heap))
(display "After delete-min, new min: ")
(display (find-min my-heap))
(newline)

;; red-black-tree.scm - Persistent Red-Black Tree implementation

(define-module (okasaki red-black-tree)
  #:export (empty-tree empty-tree? member? insert))

;; Red-Black tree represented as:
;; Empty: 'E
;; Node: (color value left-tree right-tree)
;; where color is 'R (red) or 'B (black)

(define empty-tree 'E)

(define (empty-tree? tree)
  (eq? tree 'E))

(define (make-red value left right)
  (list 'R value left right))

(define (make-black value left right)
  (list 'B value left right))

(define (color tree)
  (if (empty-tree? tree)
      'B  ; Empty trees are considered black
      (car tree)))

(define (value tree)
  (cadr tree))

(define (left tree)
  (caddr tree))

(define (right tree)
  (cadddr tree))

(define (red? tree)
  (and (not (empty-tree? tree))
       (eq? (color tree) 'R)))

(define (black? tree)
  (or (empty-tree? tree)
      (eq? (color tree) 'B)))

(define (balance-left color val left-tree right-tree)
  (cond
    ;; Case 1: Black node with red-red left grandchild
    ((and (eq? color 'B)
          (not (empty-tree? left-tree))
          (red? left-tree)
          (not (empty-tree? (left left-tree)))
          (red? (left left-tree)))
     (make-red (value left-tree)
               (make-black (value (left left-tree))
                          (left (left left-tree))
                          (right (left left-tree)))
               (make-black val (right left-tree) right-tree)))
    
    ;; Case 2: Black node with red-red right grandchild of left child
    ((and (eq? color 'B)
          (not (empty-tree? left-tree))
          (red? left-tree)
          (not (empty-tree? (right left-tree)))
          (red? (right left-tree)))
     (make-red (value (right left-tree))
               (make-black (value left-tree)
                          (left left-tree)
                          (left (right left-tree)))
               (make-black val
                          (right (right left-tree))
                          right-tree)))
    
    ;; Default: no restructuring needed
    (else
     (list color val left-tree right-tree))))

(define (balance-right color val left-tree right-tree)
  (cond
    ;; Case 3: Black node with red-red left grandchild of right child
    ((and (eq? color 'B)
          (not (empty-tree? right-tree))
          (red? right-tree)
          (not (empty-tree? (left right-tree)))
          (red? (left right-tree)))
     (make-red (value (left right-tree))
               (make-black val
                          left-tree
                          (left (left right-tree)))
               (make-black (value right-tree)
                          (right (left right-tree))
                          (right right-tree))))
    
    ;; Case 4: Black node with red-red right grandchild
    ((and (eq? color 'B)
          (not (empty-tree? right-tree))
          (red? right-tree)
          (not (empty-tree? (right right-tree)))
          (red? (right right-tree)))
     (make-red (value right-tree)
               (make-black val
                          left-tree
                          (left right-tree))
               (make-black (value (right right-tree))
                          (left (right right-tree))
                          (right (right right-tree)))))
    
    ;; Default: no restructuring needed
    (else
     (list color val left-tree right-tree))))

(define (member? x tree)
  (cond
    ((empty-tree? tree) #f)
    ((< x (value tree)) (member? x (left tree)))
    ((> x (value tree)) (member? x (right tree)))
    (else #t)))

(define (insert-helper x tree)
  (cond
    ((empty-tree? tree) (make-red x empty-tree empty-tree))
    ((< x (value tree))
     (balance-left (color tree)
                  (value tree)
                  (insert-helper x (left tree))
                  (right tree)))
    ((> x (value tree))
     (balance-right (color tree)
                   (value tree)
                   (left tree)
                   (insert-helper x (right tree))))
    (else tree)))  ; Value already exists, return unchanged

(define (insert x tree)
  (let ((result (insert-helper x tree)))
    (make-black (value result) (left result) (right result))))

;; Example usage
(display "\nRed-Black Tree Operations:\n")
(define my-tree empty-tree)
(set! my-tree (insert 5 my-tree))
(set! my-tree (insert 3 my-tree))
(set! my-tree (insert 7 my-tree))
(set! my-tree (insert 1 my-tree))
(set! my-tree (insert 9 my-tree))
(display "Contains 3? ")
(display (member? 3 my-tree))
(newline)
(display "Contains 4? ")
(display (member? 4 my-tree))
(newline)

;; leftist-heap.scm - Persistent Leftist Heap implementation

(define-module (okasaki leftist-heap)
  #:export (empty-heap empty? insert find-min delete-min merge))

;; A leftist heap is represented as either:
;; - 'E for an empty heap
;; - (rank value left-heap right-heap) for a non-empty heap
;; where the rank is the distance to the nearest empty right subtree

(define empty-heap 'E)

(define (empty? heap)
  (eq? heap 'E))

(define (rank heap)
  (if (empty? heap)
      0
      (car heap)))

(define (value heap)
  (cadr heap))

(define (left-heap heap)
  (caddr heap))

(define (right-heap heap)
  (cadddr heap))

(define (make-heap value left right)
  (if (<= (rank right) (rank left))
      (list (+ (rank right) 1) value left right)
      (list (+ (rank left) 1) value right left)))

(define (merge h1 h2)
  (cond ((empty? h1) h2)
        ((empty? h2) h1)
        (else
         (let ((v1 (value h1))
               (v2 (value h2)))
           (if (<= v1 v2)
               (make-heap v1 (left-heap h1) (merge (right-heap h1) h2))
               (make-heap v2 (left-heap h2) (merge h1 (right-heap h2))))))))

(define (insert x heap)
  (merge (list 1 x empty-heap empty-heap) heap))

(define (find-min heap)
  (if (empty? heap)
      (error "Cannot find min of an empty heap")
      (value heap)))

(define (delete-min heap)
  (if (empty? heap)
      (error "Cannot delete min of an empty heap")
      (merge (left-heap heap) (right-heap heap))))

;; Example usage
(display "\nLeftist Heap Operations:\n")
(define my-heap empty-heap)
(set! my-heap (insert 5 my-heap))
(set! my-heap (insert 3 my-heap))
(set! my-heap (insert 7 my-heap))
(set! my-heap (insert 1 my-heap))
(display "Min value: ")
(display (find-min my-heap))
(newline)
(set! my-heap (delete-min my-heap))
(display "After delete-min, new min: ")
(display (find-min my-heap))
(newline)

;; stream.scm - Lazy Stream implementation

(define-module (okasaki stream)
  #:export (stream-cons stream-car stream-cdr stream-null stream-null? 
                       stream-map stream-filter stream-append stream-fold))

;; A stream is a delayed list
;; We use promises to implement the delay

(define stream-null '())

(define (stream-null? stream)
  (null? stream))

(define (stream-cons head tail-thunk)
  (cons head (delay tail-thunk)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-map f stream)
  (if (stream-null? stream)
      stream-null
      (stream-cons (f (stream-car stream))
                  (lambda () (stream-map f (stream-cdr stream))))))

(define (stream-filter pred stream)
  (if (stream-null? stream)
      stream-null
      (let ((head (stream-car stream)))
        (if (pred head)
            (stream-cons head
                        (lambda () (stream-filter pred (stream-cdr stream))))
            (stream-filter pred (stream-cdr stream))))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                  (lambda () (stream-append (stream-cdr s1) s2)))))

(define (stream-fold f init stream)
  (if (stream-null? stream)
      init
      (stream-fold f 
                  (f init (stream-car stream))
                  (stream-cdr stream))))

;; Example: infinite stream of natural numbers
(define (naturals-from n)
  (stream-cons n (lambda () (naturals-from (+ n 1)))))

(define naturals (naturals-from 0))

;; Take the first n elements of a stream
(define (stream-take n stream)
  (if (or (= n 0) (stream-null? stream))
      '()
      (cons (stream-car stream)
            (stream-take (- n 1) (stream-cdr stream)))))

;; Example usage
(display "\nStream Operations:\n")
(display "First 10 natural numbers: ")
(display (stream-take 10 naturals))
(newline)

(display "First 5 even natural numbers: ")
(define evens (stream-filter even? naturals))
(display (stream-take 5 evens))
(newline)

(display "First 5 squares: ")
(define squares (stream-map (lambda (x) (* x x)) naturals))
(display (stream-take 5 squares))
(newline)
