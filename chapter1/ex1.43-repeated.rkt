#lang racket
(define (repeated f n)
  (if (= n 0)
      identity
      (lambda (x) (f ((repeated f (- n 1)) x))))
  )

(provide repeated)

;((repeated (lambda (x) (* x x)) 2) 5)