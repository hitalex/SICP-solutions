#lang r5rs
(#%require "stream.rkt")
(#%require "add-mul-streams.rkt")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams )))