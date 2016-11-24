#lang racket

(require "poly.rkt")
(require "generic-ops.rkt")

(install-polynomial-package)

(define (p1)
  (make-polynomial 'x (list (list 5 1) (list 0 -1))))

(define (p2)
  (make-polynomial 'x (list (list 2 1) (list 0 -1))))

(div (p1) (p2))