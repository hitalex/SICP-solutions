#lang racket

(define (square x)
  (* x x)
  )

(define (even? x)
  (= (remainder x 2) 0)
  )

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
        )
  )

(define (carmichael? n)
  (check? n 2)
  )

(define (mod? n a)
  (= (remainder (fast-expt a n) n) a)
  )

(define (check? n a)
  (if (>= a n) true
      (if (mod? n a) (check? n (+ a 1))
          (= 1 0))
        )
  )

(carmichael? 8)
(carmichael? 1105)