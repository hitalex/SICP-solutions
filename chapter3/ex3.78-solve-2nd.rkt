#lang r5rs

(#%require "stream.rkt")
(#%require "add-mul-streams.rkt")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-stream (scale-stream integrand dt)
                               int))))
  int)

(define (solve-2nd a b dt y0 dy0 dy-dt)
  (define dy (integral (delay ddy) dy0 dy-dt))
  (define y (integral dy y0 dt))
  (define ddy (add-stream (scale-stream dy a)
                          (scale-stream y b)))
  y)