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
