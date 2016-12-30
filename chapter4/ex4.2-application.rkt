#lang racket

(require "eval.rkt")

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))

(define (operand exp)
  (caddr exp))