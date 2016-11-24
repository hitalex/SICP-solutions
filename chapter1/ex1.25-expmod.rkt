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

(define (expmod base exp m)
  (remainder (fast-expt base exp) m)
  )

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false))
  )

(fast-prime? 2027 5)