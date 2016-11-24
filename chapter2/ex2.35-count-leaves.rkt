#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate (lambda (sub-tree rest)
                (cond ((null? sub-tree) rest)
                      ((not (pair? sub-tree)) (+ 1 rest))
                      (else (+ (count-leaves sub-tree) rest))))
              0
              (map identity t)))

(define tree (list 1 (list 2 3) (list 3 (list 4 5))))
(count-leaves tree)

(define (print tree)
  (display tree)
  (newline))

;(map (lambda (sub-tree) (print sub-tree)) tree)