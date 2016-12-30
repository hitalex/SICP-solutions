#lang r5rs

(#%require "error.rkt")

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-binding var val) (cons var val))
(define (get-binding-var binding) (car binding))
(define (get-binding-val binding) (cdr binding))

;; 将框架表示为变量和值的序对
(define (make-frame vars vals)
  (define (iter vars vals result)
    (if (null? vars)
        result
        (iter (cdr vars) (cdr vals) (cons (make-binding (car vars) (car vals))
                                          result))))
  (iter vars vals '()))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied." vars vals)
          (error "Too many parameters supplied." vars vals))))

(define (add-binding-to-frame! var val frame)
  (cons (make-binding var val) frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (get-binding-var (car frame)))
             (get-binding-val (car frame)))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
             (add-binding-to-frame! var val frame))
            ((eq? var (get-binding-var (car frame)))
             (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (scan frame)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (get-binding-var (car frame)))
             (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound varialbe -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))