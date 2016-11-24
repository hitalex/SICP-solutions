#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? (cdr mobile))) (cdr mobile))
        (else (+ (total-weight (left-branch mobile))
                 (total-weight (right-branch mobile))))))

(define (check-balance mobile)
  (define (get-moment branch)
    (* (branch-length branch) (total-weight branch)))
  
  (cond ((not (pair? (car mobile))) true) ; if it is a weight, it is already balanced
        ((and (check-balance (left-branch mobile))
              (check-balance (right-branch mobile))
              (= (get-moment (left-branch mobile)) (get-moment (right-branch mobile)))))))