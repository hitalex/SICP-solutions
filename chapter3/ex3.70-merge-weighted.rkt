#lang r5rs

(#%require "stream.rkt")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (cons-stream s2car
                                            (merge-weighted (stream-cdr s1)
                                                            (stream-cdr s2)
                                                            weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;(define (weight p) (+ (car p) (cadr p)))

;(define S (weighted-pairs integers integers weight))

;(display-stream-n S 130)

#|
(define tmp (weighted-pairs integers integers (lambda (p) (let ((i (car p))
                                                               (j (cadr p)))
                                                           (+ (* 2 i) (* 3 j) (* 5 i j))))))

(define S2 (stream-filter (lambda (p) (let ((i (car p))
                                            (j (cadr p)))
                                        (or (= (remainder i 2) 0)
                                            (= (remainder j 2) 0)
                                            (= (remainder i 3) 0)
                                            (= (remainder j 3) 0)
                                            (= (remainder i 5) 0)
                                            (= (remainder j 5) 0))))
                          tmp))

;(display-stream-n S2 30)
|#

(#%provide weighted-pairs)