#lang racket

(require "ex1.21-fermat.rkt")

(define (timed-prime-test n)
  ;(newline)
  ;(display n)
  (start-prime-test n (current-inexact-milliseconds))
 )

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime n (- (current-inexact-milliseconds) start-time))
      (display ""))
  )

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  )

(define (search-for-primes a b)
  (if (< a b) ((timed-prime-test a) (search-for-primes (+ a 1) b))
      (display "")
      )
  )

(search-for-primes 100000000 100000000000)