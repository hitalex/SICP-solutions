#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(provide accumulate)

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(provide flatmap)

(define (enumerate-interval m n)
  (if (> m n)
      null
      (cons m (enumerate-interval (+ m 1) n))))
(provide enumerate-interval)