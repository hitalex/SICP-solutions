#lang racket

(require "generic-ops.rkt")
(require "put-get.rkt")
(require "poly.rkt")

;; 有理数
(define (install-rational-package)
  ;; 内部函数
  (define (numer x) (car x)) ;; 分子
  (define (denom x) (cdr x)) ;; 分母
  (define (make-rat n d) ;; 由rational-package自己负责内部的数据表示
    (let ((result (reduce n d)))
      ;(displayln (car result))
      ;(displayln (cadr result))
      (cons (car result) (cadr result)))) ; 多项式除法，只需要商

  (define (add-rat x y)
    ;(displayln (numer x))
    ;(displayln (denom x))
    ;(displayln (numer y))
    ;(displayln (denom y))
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

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

(install-polynomial-package)
(install-rational-package)

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(add rf1 rf2)

;(greatest-common-divisor p1 p2)

;(div (make-polynomial 'x '((3 1) (0 1))) (make-polynomial 'x '((0 2))))

;(div (make-polynomial 'x '((1 -1) (0 1))) (make-polynomial 'x '((0 2))))

;(greatest-common-divisor (make-polynomial 'x '((3 1) (0 -1))) (make-polynomial 'x '((0 2))))

#|
(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define Q1 (mul p1 p2))
(define Q2 (mul p1 p3))
|#
;(mul p5 p6)

;(greatest-common-divisor Q1 Q2)