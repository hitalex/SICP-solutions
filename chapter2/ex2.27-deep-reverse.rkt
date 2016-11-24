#lang racket

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (cond ((null? items) null)
        ((not (pair? items)) items)
        (else (cons (deep-reverse (cdr items)) (deep-reverse (car items))))))

(deep-reverse x)