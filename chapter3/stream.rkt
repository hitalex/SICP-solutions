#lang r5rs

(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define (force delayed-object)
  (delayed-object))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (if (not (stream-null? s))
      (begin (display (stream-car s))
             (display " ")
             (display-stream (stream-cdr s)))
      #f))

;; print n chars at most
(define (display-stream-n s n)
  (if (and (not (stream-null? s)) (> n 0))
      (begin (display (stream-car s))
             (newline)
             (display-stream-n (stream-cdr s) (- n 1)))
      #f))

(#%provide delay)
(#%provide force)
(#%provide cons-stream)
(#%provide stream-car)
(#%provide stream-cdr)
(#%provide the-empty-stream)
(#%provide stream-null?)
(#%provide stream-ref)
(#%provide stream-map)
(#%provide display-stream)
(#%provide display-stream-n)

(#%provide stream-enumerate-interval)
(#%provide stream-filter)

;; some example streams
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(#%provide integers)
(#%provide integers-starting-from)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(#%provide scale-stream)