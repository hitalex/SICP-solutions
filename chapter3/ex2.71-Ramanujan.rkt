#lang r5rs

(#%require "stream.rkt")
(#%require "ex3.70-merge-weighted.rkt")

(define (weight p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* i i i)
       (* j j j))))

(define S (weighted-pairs integers integers weight))

(define (select s)
  (define (iter t)
    (if (= (weight (stream-car t))
           (weight (stream-car (stream-cdr t))))
        (begin (display (weight (stream-car t)))
               (newline)
               (iter (stream-cdr t)))
        (iter (stream-cdr t))))
  (iter s))

;(display-stream-n (select S) 10)

;(display-stream-n S 10)

(select S)