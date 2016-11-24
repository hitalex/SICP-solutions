#lang racket

(require "accumulate.rkt")

(define (enumerate-interval m n)
  (if (> m n)
      null
      (cons m (enumerate-interval (+ m 1) n))))

;(enumerate-interval 2 6)

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;(accumulate append
;            null
;            (map (lambda (i)
;       (map (lambda (j) (list i j))
;            (enumerate-interval 1 (- i 1))))
;       (enumerate-interval 1 3)))

(provide flatmap)
(provide enumerate-interval)