#lang racket

(define (make-monitored f)
  (let ((count 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) count)
            ((eq? input 'reset-count) (set! count 0))
            (else (displayln (f input))
                  (set! count (+ count 1))))))
  )

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)

(s 'how-many-calls?)

(s 'reset-count)

(s 'how-many-calls?)