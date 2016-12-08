#lang r5rs

(define (or-gate a1 a2 output)
  (not-gate (and-gate a1 a2)))