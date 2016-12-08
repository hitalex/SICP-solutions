#lang r5rs

(#%require "constraint-system.rkt")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder x z y)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier x z y)
    z))

(define (cv value)
  (let ((x (make-connector)))
    (constant value x)
    x))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))

(define F (celsius-fahrenheit-converter C))

(probe "C:" C)
(probe "F:" F)
(set-value! C 10 'user)