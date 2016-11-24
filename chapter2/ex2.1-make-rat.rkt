#lang racket

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))) (sign (or (and (> n 0) (> d 0)) (and (< n 0) (< d 0)))))
        (if sign
            (cons (/ (abs n) g) (/ (abs d) g))
            (cons (- (/ (abs n) g)) (/ (abs d) g)))))

(print-rat (make-rat 4 6))
(print-rat (make-rat -4 6))
(print-rat (make-rat 4 -6))
(print-rat (make-rat -4 -6))