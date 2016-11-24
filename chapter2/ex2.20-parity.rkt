#lang racket

(define nil '())

(define (same-parity a . l)
  (define (iter items r)
    (cond ((null? items) nil)
          ((= (remainder (car items) 2) r) (cons (car items) (iter (cdr items) r)))
          (else (iter (cdr items) r))))
  
  (let ((r (remainder a 2)))
    (cons a (iter l r))))

(same-parity 2 3 4 5 6 7 8)