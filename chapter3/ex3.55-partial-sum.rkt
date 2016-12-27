#lang r5rs

(#%require "stream.rkt")
(#%require "add-mul-streams.rkt")

(define (partial-sums s)
  (cons-stream (stream-car s) (add-stream (partial-sums s) (stream-cdr s))))

;(display (stream-ref (partial-sum integers) 3))

(#%provide partial-sums)