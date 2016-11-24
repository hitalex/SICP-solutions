#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (interaction-set set1 set2)
  (cond ((or (equal? set1 null) (equal? set2 null)) null)
        ((element-of-set? (car set1) set2) (cons (car set1) (interaction-set (cdr set1) set2)))
        (else (interaction-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((equal? set1 null) set2)
        ((equal? set2 null) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;(element-of-set? 2 (list 1 2 3 4))