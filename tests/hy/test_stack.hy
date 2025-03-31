;; test_stack.hy - Tests for stack implementation

(import [pytest]
        [pytest [raises]]
        [okasaki.stack [empty-stack empty-stack? push pop top]])

(defn test-empty-stack []
  "Test empty stack operations"
  (assert (empty-stack? empty-stack))
  (assert (= (len empty-stack) 0)))

(defn test-push []
  "Test push operation"
  (setv stack (push 1 empty-stack))
  (assert (not (empty-stack? stack)))
  (assert (= (top stack) 1))
  
  (setv stack (push 2 stack))
  (assert (= (top stack) 2)))

(defn test-pop []
  "Test pop operation"
  (setv stack (push 2 (push 1 empty-stack)))
  (setv stack (pop stack))
  (assert (= (top stack) 1))
  
  (setv stack (pop stack))
  (assert (empty-stack? stack)))

(defn test-empty-errors []
  "Test errors on empty stack"
  (with [e (raises Exception)]
    (top empty-stack)
    (assert (in "empty stack" (str e.value)))))