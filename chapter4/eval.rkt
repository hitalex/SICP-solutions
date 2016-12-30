#lang r5rs

(#%require "error.rkt")
(#%require "env.rkt")

;; NOTE：需要区分语句特殊形式和函数的区别
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; 赋值相关
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; define statements
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;; lambda clauses
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if statement
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin clauses
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; procedures
; NOTE: 对于过程的检查只能放在eval的最后，因为这里只会查看是否是pair
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-clauses clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-clauses first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; and & or
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and-or-exps exp) (cdr exp))
(define (eval-and exp env)
  (define (iter exp)
    (if (null? exp)
        'true
        (if (not (true? (eval (car exp) env)))
            'false
            (iter (cdr exp) env))))
  (iter (and-or-exps exp)))

(define (eval-or exp env)
  (define (iter exp)
    (if (null? exp)
        'false
        (if (true? (eval (car exp) env))
            'true
            (iter (cdr exp) env))))
  (iter (and-or-exps exp)))

;; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-var-defs exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (make-let var-defs body) (list 'let var-defs body))
(define (let->combination exp)
  (define (extract-var-value var-def-list result)
    (if (null? var-def-list)
        result
        (let ((var (car (car var-def-list)))
              (value (cdr (car var-def-list))))
          (extract-var-value (cdr var-def-list)
                             (cons (cons var (car result))
                                   (cons value (cdr result)))))))
  ;; 判断是否是命名let
  (if (pair? (cadr exp))
      (let ((result (extract-var-value (let-var-defs exp) (cons '() '()))))
        (let ((var-list (car result))
              (value-list (cdr result)))
          (make-begin (list (list 'define (cons 'tmp-procedure var-list) (let-body exp)) ;; 临时函数名：tmp-procedure
                            (cons 'tmp-procedure value-list)))))
      (let ((proc-name (cadr exp))
            (bindings (caddr exp))
            (proc-body (cadddr exp)))
        (let ((result (extract-var-value bindings (cons '() '()))))
          (let ((var-list (car result))
                (value-list (cdr result)))
            (make-begin (list (list 'define (cons proc-name var-list) proc-body)
                              (cons proc-name value-list))))))))

;; let*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (if (null? (cdr (let-var-defs exp)))
      (make-let (let-var-defs exp) (let-body exp))
      (make-let (cons (car (let-var-defs exp)) '())
                (let*->nested-lets (list (cdr 'let* (let-var-defs exp) (let-body exp)))))))

(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

;; 复合过程表示
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define primitive-procedures (list (list 'car car)
                                   (list 'cdr cdr)
                                   (list 'cons cons)
                                   (list 'null? null?)
                                   (list '+ +)
                                   (list '- -)
                                   (list '* *)
                                   (list '/ /)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(#%provide eval)
(#%provide the-global-environment)
(#%provide compound-procedure?)
(#%provide procedure-parameters)
(#%provide procedure-body)