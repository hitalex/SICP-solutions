#lang r5rs

(define (count-pairs x)
  (define (iter x path)
    (if (or (not (pair? x)) (visited? x path))
        0
        (begin (cons path x)
               (+ (count-pairs (car x))
                  (count-pairs (cdr x))
                  1))))

  (define (visited? x path)
    (cond ((null? path) #f)
          ((eq? x (car path) #t))
          (else (visited? x (cdr path)))))
  
  (iter x '()))

(define x (list 'a 'b))

(define z (cons x x))

(display (count-pairs z))