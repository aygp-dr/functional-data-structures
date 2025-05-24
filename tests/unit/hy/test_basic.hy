;; test_basic.hy - Simple test file for Hy code

(import [pytest])

(defn test-basic-functionality []
  "Basic test to verify that pytest can run Hy tests"
  (assert (= 1 1))
  (assert (= [1 2 3] [1 2 3]))
  (assert (in "b" "abc")))

;; More tests will be added as data structures are implemented