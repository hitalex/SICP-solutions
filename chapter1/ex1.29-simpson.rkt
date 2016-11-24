#lang racket

(define (cube x)
  (* x x x)
  )

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b)))
  )

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
  )

;(integral cube 0 1 0.0001)

(define (simpson f a b n)
  (define (add-dx x) (+ x (/ (- b a) (* n 1.0))))

  (define (get-multiplier index)
    (cond ((= index 0) 1)
          ((= index n) 1)
          (else (if (= (remainder n 2) 1)
            4
            2)))
  )
  
  (define (sum2 term a next index)
    (if (> index n)
      0
      (+ (* (term a) (get-multiplier index)) (sum2 term (next a) next (+ index 1))))
  )
  
  (* (/ (- b a) (* 3.0 n)) (sum2 f a add-dx 0))
  )

(simpson cube 0 1 200)