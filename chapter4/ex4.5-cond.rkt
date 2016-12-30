#lang racket

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clauses? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"))
            (if (cond-special-clauses? first)
                (if (true? (eval (car first)))
                    (apply (caddr first)
                           (eval (car first))))
                (make-if ...))))))