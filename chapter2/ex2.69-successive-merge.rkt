#lang racket

(require "Huffman.rkt")
(require "ex2.68-encode-symbol.rkt")

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cdr pair))
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (> (length leaf-set) 1)
      (successive-merge (adjoin-set
                         (make-code-tree (car leaf-set) (cadr leaf-set))
                         (cddr leaf-set)))
      (car leaf-set)))

;(encode (list 'A 'C 'A 'B 'B 'D 'A) (generate-huffman-tree (list (cons 'A 4) (cons 'B 2) (cons 'C 1) (cons 'D 1))))

(provide generate-huffman-tree)