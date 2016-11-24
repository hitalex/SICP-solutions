#lang racket

(require "accumulate.rkt")
(require "ex2.40-unique-pairs.rkt")

(define (make-triple n s)
  (define (not-null t)
    (not (null? t)))
  
  (filter not-null (flatmap (lambda (i)
           (map (lambda (j) (let ((k (- s (+ i j))))
                              (if (and (> k 0) (not (= i k)) (not (= j k)))
                                  (list i j k)
                                  null)))
                (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n))))

(make-triple 10 6)