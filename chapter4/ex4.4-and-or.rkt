#lang racket

(define (eval-and exp env)
  (define (iter exp)
    (if (null? exp)
        true
        (if (not (true? (eval (car exp) env)))
            false
            (iter (cdr exp) env))))
  (iter (cdr exp)))

(define (eval-or exp env)
  (define (iter exp)
    (if (null? exp)
        false
        (if (true? (eval (car exp) env))
            true
            (iter (cdr exp) env))))
  (iter (cdr exp)))

