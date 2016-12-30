#lang r5rs

(#%require "eval.rkt")

(define input-prompt ";;; M-eval input:")

(define output-prompt ";;; M-eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedures
                     (procedure-parameters object)
                     (procedure-body object)))
      (display object)))

(driver-loop)