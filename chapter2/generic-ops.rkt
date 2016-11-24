#lang racket

#|
该函数包提供通用型操作的相关函数

NOTE: 这里还没有实现不同类型之间的强制转换
|#

(require "put-get.rkt")

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

;; 通用版的gcd过程
(define (greatest-common-divisor x y)
  (apply-generic 'gcd x y))

(define (reduce x y)
  (apply-generic 'reduce x y))

(define (=zero? x)
  (apply-generic '=zero? x))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(provide attach-tag)
(provide type-tag)
(provide contents)
(provide apply-generic)

(provide add)
(provide mul)
(provide sub)
(provide div)
(provide greatest-common-divisor)
(provide reduce)