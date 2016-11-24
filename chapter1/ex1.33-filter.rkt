#lang racket

(require "ex1.21-fermat.rkt")

(define (inc x)
  (+ x 1)
  )

(define (prime? x)
  (fast-prime? x 100)
  )

(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner filter null-value term (next a) next b))
          (combiner null-value (filtered-accumulate combiner filter null-value term (next a) next b)))
          )
  )

(define (prime-sum a b)
  (filtered-accumulate + prime? 0 identity a inc b)
  )

;(prime-sum 2 10)

(define (prime-product n)
  (define (mutual-prime x)
    (= (gcd x n) 1)
    )

  (filtered-accumulate * mutual-prime 1 identity 1 inc (- n 1))
  )

(prime-product 10)