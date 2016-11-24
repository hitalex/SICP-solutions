#lang racket

(define (make-accumulator sum)

  (define (add a)
    (begin (set! sum (+ sum a))
           (displayln sum)))

  add)

(define A (make-accumulator 5))

(A 10)

(A 10)