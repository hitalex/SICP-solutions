#lang racket

(define (square x)
  (* x x)
  )

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (divides? a b)
    (= (remainder b a) 0))
  
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))
  )

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m)))
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

(provide fast-prime?)

;(smallest-divisor 19)
;(smallest-divisor 199)
;(smallest-divisor 1999)
;(smallest-divisor 19999)
;
;(fast-prime? 229 50)
;(fast-prime? 199 20)
;(fast-prime? 2000 20)
;(fast-prime? 2465 4)