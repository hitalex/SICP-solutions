#lang r5rs

(#%require "stream.rkt")
(#%require "add-mul-streams.rkt")

(define (avg a b)
  (/ (+ a b) 2))

(define (smooth s)
  (stream-map-ext avg s (stream-cdr s)))

(define smoothed-sense-data (smooth sense-data))

(define zero-crossings
  (stream-map sign-change-detector smoothed-sense-data (stream-cdr smoothed-sense-data)))