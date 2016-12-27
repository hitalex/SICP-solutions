#lang r5rs

(#%require "stream.rkt")

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit s tolerance)
  (define (iter n)
    (if (< (abs (- (stream-ref s n)
                   (stream-ref s (+ n 1)))) tolerance)
        (stream-ref s (+ n 1))
        (iter (+ n 1))))
  (iter 0))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(display (sqrt 5 0.000001))