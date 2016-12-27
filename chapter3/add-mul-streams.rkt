#lang r5rs

(#%require "stream.rkt")

(define (stream-map-ext proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map-ext
              (cons proc (map stream-cdr argstreams))))))

(define (add-stream s1 s2)
  (stream-map-ext + s1 s2))

(define (mul-streams s1 s2)
  (stream-map-ext * s1 s2))

(define (div-streams s1 s2)
  (stream-map-ext / s1 s2))

(#%provide add-stream)
(#%provide mul-streams)
(#%provide div-streams)
(#%provide stream-map-ext)