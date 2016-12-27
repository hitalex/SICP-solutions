#lang r5rs

(define n 5)

(define (make-mutex)
  (let ((cell (list n)))
    (define (the-mutex m)
      (cond ((eq? m 'aquire)
             (if (test-and-set! cell)
                 (the-mutex 'aquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell (+ (car cell) 1)))

(define (test-and-set! cell)
  (if (<= (car cell) 0)
      #t
      (begin (set-car! cell (- (car cell) 1))
             #f)))