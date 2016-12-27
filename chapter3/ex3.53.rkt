#lang r5rs

(#%require "stream.rkt")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define s (cons-stream 1 (add-stream s s)))

(display (stream-ref s 4))