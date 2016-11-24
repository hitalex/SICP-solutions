#lang racket

(define (for-each proc items)
  (proc (car items))
  (if (null? (cdr items))
      null
      (for-each proc (cdr items))))

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4))