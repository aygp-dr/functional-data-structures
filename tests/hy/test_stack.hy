;; test_stack.hy - Tests for stack implementation

(import pytest)
(import okasaki.stack :as stack)

(defn test-empty-stack []
  "Test empty stack operations"
  (assert (stack.empty-stack? stack.empty-stack))
  (assert (= (len stack.empty-stack) 0)))

(defn test-push []
  "Test push operation"
  (setv test-stack (stack.push 1 stack.empty-stack))
  (assert (not (stack.empty-stack? test-stack)))
  (assert (= (stack.top test-stack) 1))
  
  (setv test-stack (stack.push 2 test-stack))
  (assert (= (stack.top test-stack) 2)))

(defn test-pop []
  "Test pop operation"
  (setv test-stack (stack.push 2 (stack.push 1 stack.empty-stack)))
  (setv test-stack (stack.pop test-stack))
  (assert (= (stack.top test-stack) 1))
  
  (setv test-stack (stack.pop test-stack))
  (assert (stack.empty-stack? test-stack)))

(defn test-empty-errors []
  "Test errors on empty stack"
  (with [(_ Exception)]
    (stack.top stack.empty-stack)))