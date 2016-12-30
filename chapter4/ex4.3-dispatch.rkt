#lang racket

(define (evaluator)
  (define (dispatch m)
    (cond ((eq? m 'self-evaluating) process-self-evaluating)
          ((eq? m 'variable) process-variable)
          ((eq? m 'quote) process-quote)))
  dispatch)

(define (eval exp env)
  ((evaluator (car exp)) (cdr exp) env))