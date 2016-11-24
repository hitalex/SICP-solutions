#lang racket

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  ;(/ 1 (iter 1))
  (iter 1)
  )

(define (cont-frac-iter n d k)
  (define (iter i ans)
    (if (= 0 i)
        ans
        (iter (- i 1) (/ (n i) (+ (d i) ans)))))
  (/ 1 (iter k 0))
  )

;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)

;(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)

(define (fd i)
  (cond ((= 1 i) 1)
        ((= 2 i) 2)
        ((or (= (remainder i 3) 0) (= (remainder i 3) 1)) 1)
        (else (* 2 (+ 1 (/ i 3))))))

(+ 2 (cont-frac (lambda (i) 1.0) fd 10))
