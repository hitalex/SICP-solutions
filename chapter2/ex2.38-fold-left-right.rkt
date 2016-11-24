#lang racket

(require "ex2.36-accumulate-n.rkt")

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;(fold-left / 1 (list 1 2 3))

(define fold-right accumulate)

(provide fold-right)
(provide fold-left)