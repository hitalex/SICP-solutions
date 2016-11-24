#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b)))
  )

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b)))
  )

(define (inc x)
  (+ x 1)
  )

;(product identity 1 inc 5)

(define (factorial n)
  (product identity 1 inc n)
  )

;(factorial 4)

(define (calculate-pi n)
  (define (product-pi term a b index)
    (if (> b n)
        1
        (* (term a b) (product-pi term (next-a a index) (next-b b index) (+ index 1))))
    )

  (define (divide-real a b)
    (/ (* a 1.0) b)
    )

  (define (next-a a index)
    (if (even? index)        
        a
        (+ 2 a)
        )
    )

  (define (next-b b index)
    (if (even? index)
        (+ 2 b)
        b
        )
    )
  
  (* 4 (product-pi divide-real 2 3 1))
  )

(calculate-pi 1011)