#lang r5rs

(define (mestery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((tmp (cdr x)))
          (set-cdr! x y)
          (loop tmp x))))

  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define w (mestery v))

(display v)

;(display w)