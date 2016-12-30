#lang r5rs

(define (a)
  (b)
  (display 'abc))

(define (b)
  (display 'abcd))