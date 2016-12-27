#lang r5rs

(#%require "stream.rkt")

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define primes (sieve (integers-starting-from 2)))

;(display (stream-ref primes 50))

;(display (stream-ref integers 10000))

(#%provide integers)
(#%provide integers-starting-from)