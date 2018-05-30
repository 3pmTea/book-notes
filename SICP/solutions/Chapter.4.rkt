#lang racket

; ==========
;; literal constants
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; variables
(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; quote
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;; set!
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; define (var & func)
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

;; lambda
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
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

;; apply
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
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

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
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
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



(define (lookup-variable-value exp env) 'undefined)
(define (make-procedure x y z) 'undefined)
(define (primitive-procedure? x) false)
(define (apply-primitive-procedure x y) 'undefined)
(define (compound-procedure? x) false)
(define (procedure-body x) 'undefined)
(define (extend-environment x y z) 'undefined)
(define (procedure-parameters x) 'undefined)
(define (procedure-environment x) 'undefined)
(define (true? x) (eq? x true))
(define (set-variable-value! x y z) 'undefined)
(define (define-variable! x y z) 'undefined)

; ========== E4.1
(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (let ((rest-values (list-of-values-ltr (rest-operands exps) env)))
          (cons first-value rest-values)))))

(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values-rtl (rest-operands exps) env)))
        (let ((first-value (eval (first-operand exps) env)))
          (cons first-value rest-values)))))

; ========== E4.3
; dispatching utils
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (eval-4.2 exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (car exp)) ((get 'eval (car exp)) exp env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env)))
(put 'eval 'begin (lambda (exp env)
                    (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond (lambda (exp env)
                   (eval (cond->if exp) env)))

; ========== E4.4
; reuse last-exp?, first-exp, rest-exps defined for 'begin
(define (and-expressions exp) (cdr exp))

(define (eval-and exp env)
  (let ((exps (and-expressions exp)))
    (cond ((null? exps) true)
          ((last-exp? exps) (eval (car exps)))
          (else
           (let ((value (eval (first-exp exps))))
             (if (true? value)
                 (eval (rest-exps exps))
                 false))))))

(put 'eval 'and eval-and)

(define (or-expressions exp) (cdr exp))

(define (eval-or exp env)
  (let ((exps (or-expressions exp)))
    (if (null? exps)
        false
        (let ((value (eval (first-exp exps))))
          (if (true? value)
              value
              (eval (rest-exps exps)))))))

(put 'eval 'or eval-or)

; ========== E4.5
(define (arrow-clause? clause) (eq? '=> (cadr clause)))
(define (arrow-recipient clause) (caddr clause))

(define (make-application operator operands) (cons operator operands))

(define (eval-cond-4.5 exp env) (eval (cond->if-4.5 exp) env))

(define (cond->if-4.5 exp) (expand-clauses-4.5 (cond-clauses exp)))

(define (expand-clauses-4.5 clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last: COND->IF" clauses)))
              ((arrow-clause? first)
               (let ((it (cond-predicate first)))
                 (make-if it
                          (make-application (arrow-recipient first)
                                            (list it))
                          (expand-clauses rest))))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(define (install-arrow-cond)
  (put 'eval 'cond eval-cond-4.5)
  'done)

; ========== E4.6
(define (let-args exp)
  (if (named-let? exp) (caddr exp) (cadr exp)))
(define (let-body exp)
  (if (named-let? exp) (cdddr exp) (cddr exp)))
(define (let-var arg) (car arg))
(define (let-exp arg) (cadr arg))

(define (let->combination exp)
  (define (iter selector clauses)
    (if (null? clauses)
        '()
        (cons (selector (car clauses))
              (iter selector (cdr clauses)))))
  (let ((args (iter let-var (let-args exp)))
        (params (iter let-exp (let-args exp))))
    (make-application
     (make-lambda args let-body)
     params)))

(define (eval-let exp env)
  (eval (let->combination exp) env))

(put 'eval 'let eval-let)

; ========== E4.7
(define (make-let args body) (cons 'let (cons args body)))
(define (make-let* args body) (cons 'let* (cons args body)))

(define (let*->nested-lets exp)
  (define (reduce-let* args body)
    (if (null? args)
        (sequence->exp body)
        (make-let (list (car args))
                  (list (reduce-let* (cdr args) body)))))
  (reduce-let* (let-args exp) (let-body exp)))

; test:
; (let*->nested-lets '(let* ((x 1) (y 2)) x y)) -> '(let ((x 1)) (let ((y 2)) (begin x y)))
; (let*->nested-lets '(let* () 1)) -> 1

; ========== E4.8
(define (named-let? exp)
  (not (or
        (pair? (cadr exp))
        (null? (cadr exp)))))
(define (let-name exp) (and (named-let? exp) (cadr exp)))

(define (make-func-definition name args body)
  (cons 'define (cons (cons name args) body)))
(define (make-definition var value)
  (list 'define var value))

(define (let->combination-4.8 exp)
  (define (iter selector clauses)
    (if (null? clauses)
        '()
        (cons (selector (car clauses))
              (iter selector (cdr clauses)))))
  (let ((args (iter let-var (let-args exp)))
        (params (iter let-exp (let-args exp))))
    (let ((func (make-lambda args let-body)))
      (if (named-let? exp)
          (make-application
           (make-lambda '()
                        (list (make-func-definition
                               (let-name exp)
                               args
                               let-body)
                              (make-application
                               (let-name exp)
                               params)))
           '())
          (make-application
           (make-lambda args let-body)
           params)))))
