;; list.hy - Persistent List implementation
(defn empty? [lst]
  "Check if a list is empty"
  (= lst []))
(defn cons-list [x lst]
  "Construct a new list with x as head and lst as tail"
  (+ [x] lst))
(defn head [lst]
  "Get the first element of a list"
  (get lst 0))
(defn tail [lst]
  "Get all elements of a list except the first"
  (cut lst 1 None))
;; Example usage
(setv example-list (cons-list 1 (cons-list 2 (cons-list 3 []))))
(print f"List: {example-list}")
(print f"Head: {(head example-list)}")
(print f"Tail: {(tail example-list)}")
(print f"Is empty? {(empty? example-list)}")

;; stack.hy - Persistent Stack implementation

(setv empty-stack [])

(defn empty-stack? [stack]
  (= (len stack) 0))

(defn push [item stack]
  (+ [item] stack))

(defn top [stack]
  (if (empty-stack? stack)
      (raise (Exception "Cannot get top of empty stack"))
      (get stack 0)))

(defn pop [stack]
  (if (empty-stack? stack)
      (raise (Exception "Cannot pop empty stack"))
      (cut stack 1 None)))

;; Example usage
(print "\nStack Operations:")
(setv my-stack empty-stack)
(setv my-stack (push 3 my-stack))
(setv my-stack (push 2 my-stack))
(setv my-stack (push 1 my-stack))
(print f"Stack: {my-stack}")
(print f"Top: {(top my-stack)}")
(print f"After pop: {(pop my-stack)}")

;; queue.hy - Persistent Queue implementation using two lists

(setv empty-queue [[] []])

(defn empty-queue? [queue]
  (and (= (len (get queue 0)) 0) 
       (= (len (get queue 1)) 0)))

(defn check-queue [queue]
  (if (= (len (get queue 0)) 0)
      (if (= (len (get queue 1)) 0)
          empty-queue
          [(list (reversed (get queue 1))) []])
      queue))

(defn enqueue [item queue]
  (check-queue [(get queue 0) (+ (get queue 1) [item])]))

(defn front [queue]
  (if (empty-queue? queue)
      (raise (Exception "Cannot get front of empty queue"))
      (get (get queue 0) 0)))

(defn dequeue [queue]
  (if (empty-queue? queue)
      (raise (Exception "Cannot dequeue empty queue"))
      (check-queue [(list (cut (get queue 0) 1 None)) (get queue 1)])))

;; Example usage
(print "\nQueue Operations:")
(setv my-queue empty-queue)
(setv my-queue (enqueue 1 my-queue))
(setv my-queue (enqueue 2 my-queue))
(setv my-queue (enqueue 3 my-queue))
(print f"Queue: {my-queue}")
(print f"Front: {(front my-queue)}")
(setv my-queue (dequeue my-queue))
(print f"After dequeue: {my-queue}")
(print f"New front: {(front my-queue)}")

;; binomial-heap.hy - Persistent Binomial Heap implementation

(setv empty-heap [])

(defn empty-heap? [heap]
  (= (len heap) 0))

(defn rank [tree]
  (get tree 0))

(defn root-value [tree]
  (get tree 1))

(defn children [tree]
  (get tree 2))

(defn make-tree [rank value children]
  [rank value children])

(defn link [t1 t2]
  (if (< (root-value t1) (root-value t2))
      (make-tree (+ (rank t1) 1) 
                 (root-value t1) 
                 (+ [t2] (children t1)))
      (make-tree (+ (rank t2) 1) 
                 (root-value t2) 
                 (+ [t1] (children t2)))))

(defn insert-tree [tree heap]
  (if (empty-heap? heap)
      [tree]
      (let [t (get heap 0)
            ts (cut heap 1 None)]
        (if (< (rank tree) (rank t))
            (+ [tree] heap)
            (insert-tree (link tree t) ts)))))

(defn insert [x heap]
  (insert-tree (make-tree 0 x []) heap))

(defn merge [heap1 heap2]
  (cond 
    [(empty-heap? heap1) heap2]
    [(empty-heap? heap2) heap1]
    [True
     (let [t1 (get heap1 0)
           ts1 (cut heap1 1 None)
           t2 (get heap2 0)
           ts2 (cut heap2 1 None)]
       (cond
         [(< (rank t1) (rank t2))
          (+ [t1] (merge ts1 heap2))]
         [(< (rank t2) (rank t1))
          (+ [t2] (merge heap1 ts2))]
         [True
          (insert-tree (link t1 t2) (merge ts1 ts2))]))]))

(defn find-min-tree [heap]
  (if (= (len heap) 1)
      (get heap 0)
      (let [t (get heap 0)
            t' (find-min-tree (cut heap 1 None))]
        (if (< (root-value t) (root-value t'))
            t
            t'))))

(defn find-min [heap]
  (if (empty-heap? heap)
      (raise (Exception "Cannot find min of empty heap"))
      (root-value (find-min-tree heap))))

(defn remove-min-tree [heap]
  (if (= (len heap) 1)
      [(get heap 0) []]
      (let [rest-result (remove-min-tree (cut heap 1 None))
            t' (get rest-result 0)
            ts' (get rest-result 1)
            t (get heap 0)]
        (if (< (root-value t) (root-value t'))
            [t (cut heap 1 None)]
            [t' (+ [t] ts')]))))

(defn delete-min [heap]
  (if (empty-heap? heap)
      (raise (Exception "Cannot delete min from empty heap"))
      (let [result (remove-min-tree heap)
            min-tree (get result 0)
            rest-heap (get result 1)
            rev-children (list (reversed (children min-tree)))]
        (merge rev-children rest-heap))))

;; Example usage
(print "\nBinomial Heap Operations:")
(setv my-heap empty-heap)
(setv my-heap (insert 5 my-heap))
(setv my-heap (insert 3 my-heap))
(setv my-heap (insert 7 my-heap))
(setv my-heap (insert 1 my-heap))
(print f"Min value: {(find-min my-heap)}")
(setv my-heap (delete-min my-heap))
(print f"After delete-min, new min: {(find-min my-heap)}")

;; red-black-tree.hy - Persistent Red-Black Tree implementation

(setv EMPTY 'E)

(defn empty-tree? [tree]
  (= tree EMPTY))

(defn make-red [value left right]
  ['R value left right])

(defn make-black [value left right]
  ['B value left right])

(defn color [tree]
  (if (empty-tree? tree)
      'B  ; Empty trees are considered black
      (get tree 0)))

(defn value [tree]
  (get tree 1))

(defn left [tree]
  (get tree 2))

(defn right [tree]
  (get tree 3))

(defn red? [tree]
  (and (not (empty-tree? tree))
       (= (color tree) 'R)))

(defn black? [tree]
  (or (empty-tree? tree)
      (= (color tree) 'B)))

(defn balance-left [color val left-tree right-tree]
  (cond
    ;; Case 1: Black node with red-red left grandchild
    [(and (= color 'B)
          (not (empty-tree? left-tree))
          (red? left-tree)
          (not (empty-tree? (left left-tree)))
          (red? (left left-tree)))
     (make-red (value left-tree)
               (make-black (value (left left-tree))
                          (left (left left-tree))
                          (right (left left-tree)))
               (make-black val (right left-tree) right-tree))]
    
    ;; Case 2: Black node with red-red right grandchild of left child
    [(and (= color 'B)
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
                          right-tree))]
    
    ;; Default: no restructuring needed
    [True
     [color val left-tree right-tree]]))

(defn balance-right [color val left-tree right-tree]
  (cond
    ;; Case 3: Black node with red-red left grandchild of right child
    [(and (= color 'B)
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
                          (right right-tree)))]
    
    ;; Case 4: Black node with red-red right grandchild
    [(and (= color 'B)
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
                          (right (right right-tree))))]
    
    ;; Default: no restructuring needed
    [True
     [color val left-tree right-tree]]))

(defn member? [x tree]
  (cond
    [(empty-tree? tree) False]
    [(< x (value tree)) (member? x (left tree))]
    [(> x (value tree)) (member? x (right tree))]
    [True True]))

(defn insert-helper [x tree]
  (cond
    [(empty-tree? tree) (make-red x EMPTY EMPTY)]
    [(< x (value tree))
     (balance-left (color tree)
                  (value tree)
                  (insert-helper x (left tree))
                  (right tree))]
    [(> x (value tree))
     (balance-right (color tree)
                   (value tree)
                   (left tree)
                   (insert-helper x (right tree)))]
    [True tree]))  ; Value already exists, return unchanged

(defn insert [x tree]
  (let [result (insert-helper x tree)]
    (make-black (value result) (left result) (right result))))

;; Example usage
(print "\nRed-Black Tree Operations:")
(setv my-tree EMPTY)
(setv my-tree (insert 5 my-tree))
(setv my-tree (insert 3 my-tree))
(setv my-tree (insert 7 my-tree))
(setv my-tree (insert 1 my-tree))
(setv my-tree (insert 9 my-tree))
(print f"Contains 3? {(member? 3 my-tree)}")
(print f"Contains 4? {(member? 4 my-tree)}")

;; Additional utility functions
(defn in-order [tree]
  "Traverse the tree in-order and return a list of values"
  (if (empty-tree? tree)
      []
      (+ (in-order (left tree))
         [(value tree)]
         (in-order (right tree)))))

(defn height [tree]
  "Calculate the height of the tree"
  (if (empty-tree? tree)
      0
      (+ 1 (max (height (left tree))
                (height (right tree))))))

(defn black-height [tree]
  "Calculate the black height of the tree"
  (if (empty-tree? tree)
      1  ; Empty nodes are black
      (+ (if (= (color tree) 'B) 1 0)
         (black-height (left tree)))))

(defn verify-red-black-properties [tree]
  "Verify that the tree satisfies red-black properties"
  (defn no-red-red? [t]
    (if (empty-tree? t)
        True
        (and (or (not (red? t))
                (and (black? (left t))
                     (black? (right t))))
             (no-red-red? (left t))
             (no-red-red? (right t)))))
  
  (defn black-balanced? [t]
    (defn black-path-lengths [t]
      (if (empty-tree? t)
          #{0}  ; Set containing 0
          (let [child-lengths (+ (black-path-lengths (left t))
                               (black-path-lengths (right t)))
                increment (if (black? t) 1 0)]
            (set (map (fn [x] (+ x increment)) child-lengths)))))
    
    (= (len (black-path-lengths t)) 1))  ; All paths have the same black length
  
  (and (black? tree)  ; Root is black
       (no-red-red? tree)  ; No red node has a red child
       (black-balanced? tree)))  ; All paths from root to leaf have same number of black nodes

;; leftist-heap.hy - Persistent Leftist Heap implementation

(setv EMPTY 'E)

(defn empty? [heap]
  (= heap EMPTY))

(defn rank [heap]
  (if (empty? heap)
      0
      (get heap 0)))

(defn value [heap]
  (get heap 1))

(defn left-heap [heap]
  (get heap 2))

(defn right-heap [heap]
  (get heap 3))

(defn make-heap [value left right]
  (if (<= (rank right) (rank left))
      [(+ (rank right) 1) value left right]
      [(+ (rank left) 1) value right left]))

(defn merge [h1 h2]
  (cond 
    [(empty? h1) h2]
    [(empty? h2) h1]
    [True
     (let [v1 (value h1)
           v2 (value h2)]
       (if (<= v1 v2)
           (make-heap v1 (left-heap h1) (merge (right-heap h1) h2))
           (make-heap v2 (left-heap h2) (merge h1 (right-heap h2)))))]))

(defn insert [x heap]
  (merge [1 x EMPTY EMPTY] heap))

(defn find-min [heap]
  (if (empty? heap)
      (raise (Exception "Cannot find min of an empty heap"))
      (value heap)))

(defn delete-min [heap]
  (if (empty? heap)
      (raise (Exception "Cannot delete min of an empty heap"))
      (merge (left-heap heap) (right-heap heap))))

;; Example usage
(print "\nLeftist Heap Operations:")
(setv my-heap EMPTY)
(setv my-heap (insert 5 my-heap))
(setv my-heap (insert 3 my-heap))
(setv my-heap (insert 7 my-heap))
(setv my-heap (insert 1 my-heap))
(print f"Min value: {(find-min my-heap)}")
(setv my-heap (delete-min my-heap))
(print f"After delete-min, new min: {(find-min my-heap)}")

;; stream.hy - Lazy Stream implementation

;; In Hy, we'll use Python's generators for lazy evaluation

(defn stream-cons [head tail-gen]
  "Create a generator that yields head followed by values from tail-gen"
  (fn []
    (yield head)
    (for [x (tail-gen)]
      (yield x))))

(defn stream-car [stream]
  "Get the first element from a stream"
  (next (iter (stream))))

(defn stream-take [n stream]
  "Take the first n elements of a stream"
  (list (take n (stream))))

(defn stream-map [f stream]
  "Apply function f to each element in the stream"
  (fn []
    (for [x (stream)]
      (yield (f x)))))

(defn stream-filter [pred stream]
  "Filter stream to only include elements that satisfy pred"
  (fn []
    (for [x (stream)]
      (when (pred x)
        (yield x)))))

(defn stream-append [s1 s2]
  "Concatenate two streams"
  (fn []
    (for [x (s1)]
      (yield x))
    (for [x (s2)]
      (yield x))))

;; Example: infinite stream of natural numbers
(defn naturals-from [n]
  "Generate natural numbers from n"
  (fn []
    (setv current n)
    (while True
      (yield current)
      (setv current (+ current 1)))))

;; Example usage
(print "\nStream Operations:")
(setv naturals (naturals-from 0))
(print f"First 10 natural numbers: {(stream-take 10 naturals)}")

(setv evens (stream-filter (fn [x] (= (% x 2) 0)) (naturals-from 0)))
(print f"First 5 even natural numbers: {(stream-take 5 evens)}")

(setv squares (stream-map (fn [x] (* x x)) (naturals-from 0)))
(print f"First 5 squares: {(stream-take 5 squares)}")
