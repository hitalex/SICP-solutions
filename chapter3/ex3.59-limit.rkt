#lang r5rs

(#%require "stream.rkt")
(#%require "add-mul-streams.rkt")

(define ones (cons-stream 1 ones))

(define (integrate-series a)
  (mul-streams a (div-streams ones integers)))

(define exp-series (cons-stream 1 (integrate-series exp-series)))