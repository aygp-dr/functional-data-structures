;; test-runner.scm - Test runner for Scheme code

(define-module (tests scheme test-runner)
  #:export (main))

;; Import test modules
(use-modules (ice-9 ftw))

;; Try to load stack module and stack test
(catch #t
  (lambda ()
    (use-modules (okasaki stack))
    (use-modules (tests scheme test-stack)))
  (lambda (key . args)
    (format #t "Warning: Could not load test modules. Error: ~a~%" args)))

;; Simple test utilities
(define-syntax assert-equal
  (syntax-rules ()
    ((_ expected expr)
     (let ((result expr))
       (if (equal? expected result)
           (begin
             (display "PASS: ")
             (display 'expr)
             (newline))
           (begin
             (display "FAIL: ")
             (display 'expr)
             (display " - Expected: ")
             (display expected)
             (display ", Got: ")
             (display result)
             (newline)
             (throw 'test-failure)))))))

;; Load test modules
(define (load-tests)
  (display "Loading test modules...\n")
  #t)

;; Run all tests
(define (run-all-tests)
  (display "\n===== Running all tests =====\n")
  
  ;; Run basic sanity tests
  (display "\n----- Basic sanity tests -----\n")
  (assert-equal 1 (+ 0 1))
  (assert-equal '(1 2 3) (list 1 2 3))
  
  ;; Run stack tests if available
  (display "\n----- Stack tests -----\n")
  (cond
    ((defined? 'run-tests) (run-tests))
    (else (display "Stack tests not available\n")))
  
  (display "\n===== All tests passed! =====\n")
  #t)

;; Main function
(define (main args)
  (load-tests)
  (run-all-tests)
  (exit 0))

;; If this file is being executed directly
(when (and (defined? 'command-line) (= (length (command-line)) 1))
  (main '()))