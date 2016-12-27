#lang r5rs


(#%require "stream.rkt")
(#%require "add-mul-streams.rkt")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-stream (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-stream (scale-stream i R)
                (integral (scale-stream i (/ 1 C))
                          v0
                          dt))))

(define RC1 (RC 5 1 0.5))

(display-stream-n (RC1 integers 1) 100)