#lang racket

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess x)
    (if (good-enough? guess x)
        guess
        (iter (improve-guess guess x) x)))
  
  (lambda (guess x) (iter guess x))
  )

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) 0.0001)
    )

  (define (improve-guess guess x)
    (average guess (/ x guess)))
  
  ((iterative-improve good-enough? improve-guess) 1.0 x)
  )

(sqrt 9)