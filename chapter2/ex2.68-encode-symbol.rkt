#lang racket

(require "Huffman.rkt")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'C 1)
                    (make-leaf 'D 1)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol ch tree)
  (define (encode-symbol-iter ch curr)
    (if (leaf? curr)
        '()
        (cond ((element-of-set? ch (symbols (left-branch curr)))
               (cons '0 (encode-symbol-iter ch (left-branch curr))))
              ((element-of-set? ch (symbols (right-branch curr)))
               (cons '1 (encode-symbol-iter ch (right-branch curr))))
              (else (error "Letter not found.")))))

  (encode-symbol-iter ch tree))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;(encode (list 'A 'C 'A 'B 'B 'D 'A) sample-tree)

(provide encode)

;sample-tree