#lang r5rs

(#%require "constraint-system.rkt")

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(define (averger a b c)
  (let ((sum (make-connector))
        (const (make-connector)))
    (adder a b sum)
    (constant 2 const)
    (multiplier c const sum)
    'ok))


(averger a b c)
(probe "Adder 1:" a)
(probe "Adder 2:" b)
(probe "Avg." c)

(set-value! a 3 'user)
(set-value! c 10 'user)