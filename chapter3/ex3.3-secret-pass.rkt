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
    (cond ((not (eq? passwd user-passwd)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"))))

  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'other-secret-password 'withdraw) 40)