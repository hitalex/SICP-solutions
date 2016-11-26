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
  (define (iter p q)
    (cond ((or (null? p) (null? q)) #f)
          ((eq? p q) #t)
          (else (begin (set! p (cdr p))
                       (if (null? (cdr q))
                           #f
                           (begin (set! q (cdr (cdr q)))
                                  (iter p q)))))))
  (iter x (cdr x)))


(define x (list 'a 'b))
(define y (list 'c 'd))
;(set-cdr! x y)
;(set-cdr! y x)

;(display x)
(display (has-loop? z))