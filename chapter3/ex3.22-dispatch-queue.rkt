#lang r5rs

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (display "ERROR: empty queue -- FRONT-QUEUE")
          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (display "Error: empty queue -- DELETE QUEUE"))
            (else
             (set! front-ptr (cdr front-ptr)))))

    (define (print-queue)
      (define (print-iter curr)
        (if (not (null? curr))
            (begin (display (car curr))
                   (print-iter (cdr curr)))))
      
      (print-iter front-ptr))
    
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (display "Error: unknown requet!"))))

    dispatch))

(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))
(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue) (queue 'front-queue))
(define (insert-queue! queue) (queue 'insert-queue!))
(define (delete-queue! queue) (queue 'delete-queue!))

(define q (make-queue))

((q 'insert-queue!) 'a)
((q 'insert-queue!) 'b)
((q 'print-queue))
((q 'delete-queue!))
((q 'delete-queue!))
((q 'delete-queue!))
((q 'print-queue))