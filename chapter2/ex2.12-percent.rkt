#lang racket

(define (make-center-percent center p)
  (make-interval (* center (- 1 p)) (* center (+ 1 p))))

(define (percent i)
  (let ((q (/ (lower-bound i) (upper-bound i))))
    (/ (+ q 1) (- 1 q))))