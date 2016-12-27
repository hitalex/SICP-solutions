#lang racket

(#%require "stream.rkt")
(#%require "add-mul-streams.rkt")

(define (sign-change-detector a b)
  (cond ((and (> a 0) (< b 0)) -1)
        ((and (< a 0) (> b 0)) 1)
        (else 0)))

(define zero-crossings
  (stream-map-ext sign-change-detector sense-data (stream-cdr sense-data)))