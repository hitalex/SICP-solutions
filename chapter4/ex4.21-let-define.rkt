#lang r5rs

(display ((lambda (n)
           ((lambda (fact)
              (fact fact n))
            (lambda (ft k)
              (if (= k 1)
                  1
                  (* k (ft ft (- k 1)))))))
           10))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))

(display (f 11))