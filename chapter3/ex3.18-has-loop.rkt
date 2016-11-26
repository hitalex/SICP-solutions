#lang r5rs

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (has-loop? x)
  (define visited '())
  (define (iter x)
    (cond ((null? x) #f)
          ((has-visited? x visited) #t)
          (else (begin (set! visited (cons x visited))
                       (iter (cdr x))))))

  (define (has-visited? x visited)
    (cond ((null? visited) #f)
          ((eq? x (car visited)) #t)
          (else (has-visited? x (cdr visited)))))
  
  (iter x))


(define x (cons 'a 'b))
(define y (cons 'c 'd))
(set-cdr! x y)
(set-cdr! y x)

;(display x)
(display (has-loop? z))