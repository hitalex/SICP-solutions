#lang racket

(require "ex2.36-accumulate-n.rkt")

(define mat (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;(dot-product (list 1 2 3) (list 3 4 5))

(define (matrix-*-vector m v)
  (map (lambda (t) (dot-product t v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

;(transpose mat)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (t) (map (lambda (l) (dot-product t l)) cols)) m)))

(matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 1) (list 3)))
