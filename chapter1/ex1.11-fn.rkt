#lang racket

(define (f n)
  (cond ((< n 3) n)
        (else (+ (+ (f (- n 1)) (* (f (- n 2)) 2)) (* (f (- n 3)) 3)))
   )
  )


;(define (f2 n)
;  (define (f a b c count)
;    (if (= count 0)
;        c
;        (f )
;        )
;    )
;  )

(f 1)
(f 5)