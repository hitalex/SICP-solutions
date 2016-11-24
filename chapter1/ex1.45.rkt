#lang racket

(require "ex1.36-fixed-point-log.rkt")
(require "ex1.43-repeated.rkt")

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
  )

;(sqrt 3)

(define (root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (* y y y y y y y)))) 1.0)
  )

(root 10)