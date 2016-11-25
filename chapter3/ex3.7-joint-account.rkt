#lang racket

(define (make-account balance user-passwd)  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds."))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch passwd m)
    (cond ((eq? m 'test-passwd) (eq? passwd user-passwd)) ;; test whether passwd is correct
          ((not (eq? passwd user-passwd)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"))))

  dispatch)

(define (make-joint acc passwd new-passwd)
  (if (acc passwd 'test-passwd)
      (lambda (passwd, m)
        (begin (set! user-passwd new-passwd)
               ))
      (error "Incorrect password")))

(define acc (make-account 100 'secret-password))

