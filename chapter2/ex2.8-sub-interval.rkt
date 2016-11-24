#lang racket

(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define (sub-interval x y)
  (make-interval (- (car x) (car y)) (- (cdr x) (cdr y))))