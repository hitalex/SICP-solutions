#lang racket

; 该package用于提供基于hashtable的put和get

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(provide put)
(provide get)