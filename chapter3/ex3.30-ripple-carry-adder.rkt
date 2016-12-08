#lang r5rs

(define (ripple-carry-adder A B S c)
  (define (iter A B S c)
    (if (or (null? A) (null? B) (null? S))
        c
        (let ((out-c (make-wire)))
          (begin (full-adder (car A) (car B) (car S) new-c)
               (iter (cdr A) (cdr B) (cdr S) new-c)))))

  (iter A B S c))