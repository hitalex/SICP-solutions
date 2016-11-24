#lang racket

(define (variable? exp)
  (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2) (list '+ a1 a2))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;(define (make-product a1 a2) (list '* a1 a2))
(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2) (* a1 a2)))
        (else (list '* a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s)
  (if (= (length (cddr s)) 1)
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (= (length (cddr p)) 1)
      (caddr p)
      (cons '* (cddr p))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp)
                                 (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;(deriv '(* x y) 'x)