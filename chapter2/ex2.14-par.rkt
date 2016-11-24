#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (print-interval x)
  (newline)
  (display "(")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display ")")
  )

(define (make-center-percent center p)
  (make-interval (* center (- 1 p)) (* center (+ 1 p))))

(define (percent i)
  (let ((q (/ (lower-bound i) (upper-bound i))))
    (/ (+ q 1) (- 1 q))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;(print-interval (make-center-percent 3 0.1))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;(let ((r1 (make-center-percent 6 0.1))
;      (r2 (make-center-percent 4 0.2)))
;  (print-interval (par1 r1 r2))
;  (newline)
;  (print-interval (par2 r1 r2)))

(let ((r1 (make-center-percent 6 0.001))
      (r2 (make-center-percent 4 0.002)))
  (print-interval (div-interval r1 r1))
  (newline)
  (print-interval (div-interval r1 r2)))