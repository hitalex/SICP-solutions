#lang racket

(define (double x)
  (* x 2)
  )

(define (halve x)
  (/ x 2)
  )

(define (mul a b)
  (define (even? x)
    (= (remainder x 2) 0)
    )
  
  (cond ((= a 0) 0)
        ((= a 1) b)
        ((even? a) (double (mul (halve a) b)))        
        (else (+ b (mul (- a 1) b)))
    )
  )

(mul 4 2)

(mul 9 3)