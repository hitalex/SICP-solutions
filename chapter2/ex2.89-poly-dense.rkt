#lang racket

; 稠密多项式的表示和计算

(require "put-get.rkt")
(require "generic-ops.rkt")

; 该函数包提供关于稠密多项式的计算实现，包括加、乘

;; =zero? 函数应该可以处理不同类型的数据
(define (=zero? x)
  (= x 0))

(define (install-polynomial-package)
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))

  (define (variable? e) (symbol? e))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))

  ;; representations of terms and term list
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (adjoin-termlist (+ (first-term L1) (first-term L2))
                            (add-terms (rest-terms L1) (rest-terms L2))))))

  (define (sub-terms L1 L2)
    (add-terms L1 (negative L2)))

  ;; 对整个termlist取负
  (define (negative L)
    (if (empty-termlist? L)
        '()
        (let ((t (first-term L)))
          (adjoin-term (- t)
                       (negative (rest-terms L))))))

  (define (mul-terms L1 L2)
    (define (mul-terms-iter L1 L2 order-zeros) ; order-zeros 表示应该在后面增加的零的列表
      (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (append order-zeros
                           (mul-term-by-all-terms (first-term L1) L2))
                   (mul-terms-iter (rest-terms L1) L2 (append order-zeros (list 0))))))

    (mul-terms-iter L1 L2 '()))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (* t1 t2)
                       (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (adjoin-term term term-list)
    (cons term term-list))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list)) ; lowest 
  (define (rest-terms term-list) (cdr term-list)) ; other terms except for the lowest term
  (define (empty-termlist? term-list) (null? term-list))

  (define (adjoin-termlist term term-list)
    (cons term term-list))

  ;; operations of poly
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  
  'done)

(define (make-polynomial var term)
  ((get 'make 'polynomial) var term))

(install-polynomial-package)

(define (p1)
  (make-polynomial 'x (list -5 -2 3 0 2 1)))

(define (p2)
  (make-polynomial 'x (list 2 0 4)))

(mul (p1) (p2))