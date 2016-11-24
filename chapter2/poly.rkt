#lang racket

(require "put-get.rkt")
(require "generic-ops.rkt")

; 该函数包提供关于多项式的计算实现，包括加、乘

;; =zero? 函数应该可以处理不同类型的数据
(define (=zero? x)
  (= x 0))

(define (install-polynomial-package)
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))

  (define (variable? e) (symbol? e))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))

  ;; representations of terms and term list
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms (rest-terms L2) L1)))
                   (else ;; same order between t1 and t2
                    (adjoin-term
                     (make-term (order t1)
                                ;(add (coeff t1) (coeff t2)))
                                (+ (coeff t1) (coeff t2))) ;; 先用+做实验
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (add-terms L1 (negative L2)))

  ;; 对整个termlist取负
  (define (negative L)
    (if (empty-termlist? L)
        '()
        (let ((t (first-term L)))
          (adjoin-term (make-term (order t)
                                  (- (coeff t)))
                       (negative (rest-terms L))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2)) ;; 这里假设变量的阶数为常规的数
                      ;(mul (coeff t1) (coeff t2)))
                      (* (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ; termlist相除，返回商式和余式
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (/ (coeff t1) (coeff t2))) ; /除法可能出现分数
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result (div-terms
                                       (sub-terms L1
                                                  (mul-terms (adjoin-term (make-term new-o new-c)
                                                                          (the-empty-termlist))
                                                             L2))
                                       L2)))
                  ;; 形成最后的结果
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                        (cadr rest-of-result))
                  ))))))

  (define (gcd-terms L1 L2)
    ;(displayln "Gcd terms call:")
    ;(displayln L1)
    ;(displayln L2)
   (if (empty-termlist? L2)
        (divide-coeff-gcd L1)
        (let ((r (pseudoremainder-terms L1 L2)))
          (gcd-terms L2 r))))

  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))

  (define (pseudoremainder-terms L1 L2)
    (let ((o1 (order (first-term L1)))
          (o2 (order (first-term L2)))
          (c (coeff (first-term L2))))
      (let ((m (expt c (- (+ 1 o1) o2))))
        (let ((tm (adjoin-term (make-term 0 m) (the-empty-termlist))))
          (cadr (div-terms (mul-terms L1 tm)
                           L2))))))

  ; 简化最终的结果，先求出各个系数的gcd，然后再除以它
  (define (divide-coeff-gcd L)
    ; g is the curr gcd
    (define (divide-coeff-gcd-iter L curr)
      (if (empty-termlist? L)
          (cons curr '()) ;; (最终的公约数，除以公约数后的结果)
          (let ((result (divide-coeff-gcd-iter (rest-terms L)
                                               (gcd curr (coeff (first-term L))))))
            (let ((g (car result))
                  (t (cdr result)))
              (cons g ; the gcd of all coeff
                    (cons (make-term (order (first-term L)) (/ (coeff (first-term L)) g)) ; the partial result
                          t))))))

    (cdr (divide-coeff-gcd-iter L (coeff (first-term L)))))

  (define (reduce-terms L1 L2)
    (let ((g (gcd-terms L1 L2)))
      (list (car (div-terms L1 g)) (car (div-terms L2 g)))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; operations of poly
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (reduce-terms (term-list p1)
                                    (term-list p2))))
          (list (tag (make-poly (variable p1) (car result)))
                (tag (make-poly (variable p2) (cadr result)))))
        (error "Polys not in same var -- REDUCE-POLY"
               (list p1 p2))))

  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (reduce-poly p1 p2)))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  
  'done)

(define (make-polynomial var term)
  ((get 'make 'polynomial) var term))

(provide install-polynomial-package)
(provide make-polynomial)