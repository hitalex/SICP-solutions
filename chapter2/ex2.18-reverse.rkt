#lang racket

(define (reverse items)
  (if (null? (cdr items))
      (car items)
      (cons (reverse (cdr items)) (car items))))

(reverse (list 1 4 9 16))