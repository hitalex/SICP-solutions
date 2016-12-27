#lang r5rs

(#%require "stream.rkt")

(define (show x)
  (display x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

;(display (stream-ref x 5))