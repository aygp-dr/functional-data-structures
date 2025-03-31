;; test-stack.scm - Tests for stack implementation

(define-module (tests scheme test-stack)
  #:use-module (okasaki stack))

;; Test procedures
(define (run-stack-tests)
  (display "Running stack tests...\n")
  
  ;; Create a new stack
  (let ((stack empty-stack))
    ;; Test empty stack
    (assert (empty-stack? stack) "New stack should be empty")
    
    ;; Test push
    (let ((stack2 (push 1 stack)))
      (assert (not (empty-stack? stack2)) "Stack after push should not be empty")
      (assert (= (top stack2) 1) "Top element should be 1")

      ;; Test push multiple elements
      (let ((stack3 (push 2 stack2)))
        (assert (= (top stack3) 2) "Top element should be 2")
        
        ;; Test pop
        (let ((stack4 (pop stack3)))
          (assert (= (top stack4) 1) "Top element after pop should be 1")
          (assert (not (empty-stack? stack4)) "Stack after one pop should not be empty")
          
          ;; Test pop to empty
          (let ((stack5 (pop stack4)))
            (assert (empty-stack? stack5) "Stack after all pops should be empty")
            
            ;; Test error handling for empty stack
            (let ((error? #f))
              (with-exception-handler
                (lambda (exn) (set! error? #t))
                (lambda () (top stack5)))
              (assert error? "Top on empty stack should throw an error"))))))
  
  (display "All stack tests passed!\n"))

;; Helper for assertions
(define (assert condition message)
  (if (not condition)
      (begin
        (display "FAIL: ")
        (display message)
        (newline)
        (error "Test failed"))
      (begin
        (display "PASS: ")
        (display message)
        (newline))))

;; Export the test runner
(define-public run-tests run-stack-tests)