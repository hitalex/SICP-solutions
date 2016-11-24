#lang racket

(define (square n)
  (* n n)
  )

(define (smallest-divisor n)
  (find-divisor n 2)
  )

(define (find-divisor n test-divisor)
  (define (next-divisor curr)
    (cond ((= curr 2) 3)
          (else (+ curr 2)))
    )
  
  (cond ((> (square test-divisor) n) n)
        ((divide? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))
        )
  )

(define (divide? a b)
  (= (remainder b a) 0)
  )

(define (prime? n)
  (= n (smallest-divisor n))
  )

(prime? 10)
(prime? 13)