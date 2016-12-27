#lang r5rs

(#%require "stream.rkt")

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(display-stream-n (expand 3 8 10) 10)