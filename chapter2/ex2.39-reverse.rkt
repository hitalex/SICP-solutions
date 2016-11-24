#lang racket

(require "ex2.38-fold-left-right.rkt")

(define (reverse sequence)
  (fold-right (lambda (x y) (cons y x)) null sequence))

(reverse (list 1 2 3))