#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
    )
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          guess
          (try next)))
    )
  (try first-guess)
  )

;(fix-point (lambda (x) (/ (+ x 1) x)) 1.0)

(provide fixed-point)