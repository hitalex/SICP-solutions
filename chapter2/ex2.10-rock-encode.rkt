#lang racket

(require "ex2.69-successive-merge.rkt")
(require "ex2.68-encode-symbol.rkt")

(define rock-tree
  (generate-huffman-tree (list (cons 'A 2)
                               (cons 'NA 16)
                               (cons 'BOOM 1)
                               (cons 'SHA 3)
                               (cons 'GET 2)
                               (cons 'YIP 9)
                               (cons 'JOB 2)
                               (cons 'WAH 1))))

;(encode '(GET A JOB) rock-tree)
(encode '(SHA NA NA NA NA NA NA NA NA) rock-tree)