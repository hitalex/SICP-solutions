#lang racket

(require "utils.rkt")

(define (queens board-size)
  (define empty-board null)

  (define (adjoin-position new-row k rest-of-queens)
    (cons (cons new-row k) rest-of-queens))

  (define (get-row pos)
    (car pos))
  (define (get-col pos)
    (cdr pos))

  (define (safe? k positions)
    (define (safe-iter last others)
      (cond ((null? others) true)
            ((conflict? last (car others)) false)
            (else (safe-iter last (cdr others)))))

    (define (conflict? last curr)
      (or (= (get-row last) (get-row curr))
          (= (- (get-row last) (get-col last))
             (- (get-row curr) (get-col curr)))
          (= (+ (get-row last) (get-col last))
             (+ (get-row curr) (get-col curr)))))

        (safe-iter (car positions) (cdr positions)))
     
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  
  (queen-cols board-size))

(queens 8)