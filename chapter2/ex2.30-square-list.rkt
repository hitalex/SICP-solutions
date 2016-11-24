#lang racket

(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* (car tree) (car tree)))
        (else (cons (square (car tree))
                    (square (cdr tree))))))

