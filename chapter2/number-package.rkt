#lang racket

(require "generic-ops.rkt")
(require "put-get.rkt")

;; 这里存放scheme-number、rational以及complex的安装过程

;; 常规的scheme数值
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (define (reduce-integers x y)
    (let ((g (gcd x y)))
      (list (/ x g) (/ y g))))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'gcd '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (x y) (reduce-integers x y)))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(provide install-scheme-number-package)
(provide make-scheme-number)

;; 有理数
(define (install-rational-package)
  ;; 内部函数
  (define (numer x) (car x)) ;; 分子
  (define (denom x) (cdr x)) ;; 分母
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; 对外接口
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  'done
  )

(define (make-rational n d)
  ((get 'make 'rational) n d))

(provide install-rational-package)
(provide make-rational)

;; 缺少复数的相关表示和计算