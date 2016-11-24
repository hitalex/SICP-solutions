#lang racket

(define x (list (list 1 (list 5 6)) (list 3 4)))

(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(fringe x)