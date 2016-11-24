#lang racket

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (begin ;(displayln (/ trials-passed trials))
                                    (/ trials-passed trials)))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))

  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (* (- x2 x1) (- y2 y1))
     (monte-carlo trials P)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (make-P x1 x2 y1 y2)
  (let ((x0 (/ (+ x1 x2) 2))
        (y0 (/ (+ y1 y2) 2)))
    (lambda ()
      (let ((x (random-in-range x1 x2))
            (y (random-in-range y1 y2)))
        (begin ;(display x)
               ;(display y)
               ;(displayln "")
               (<= (+ (sqr (- x x0))
                      (sqr (- y y0)))
                   1))))))

(define x1 2)
(define x2 8)
(define y1 4)
(define y2 10)
(define P (make-P x1 x2 y1 y2))

(estimate-integral P x1 x2 y1 y2 10000)