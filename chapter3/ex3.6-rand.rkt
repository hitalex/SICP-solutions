#lang racket

(define random-init 1)

(define rand-update random)

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'reset) (lambda (new-x) (set! x new-x)))
            ((eq? m 'generate) (begin (set! x (rand-update x))
                                      x))
            (else (error "Unknown msg"))))))