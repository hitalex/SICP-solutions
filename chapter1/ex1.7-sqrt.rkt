#lang racket

(define (average x y)
  (/ (+ x y) 2)
  )
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))
        )
  )

(define (sqrt x)
  (define (good-enough? pre curr)
    (< (/ (abs (- pre curr)) pre) 0.001)
    )

  (define (improve guess)
    (average guess (/ x guess))
    )

  (define (sqrt-iter pre guess)
    (if (good-enough? pre guess)
        guess
        (sqrt-iter guess (improve guess))
        )
    )
  (sqrt-iter x 1.0)
)

(sqrt 0.000012)
