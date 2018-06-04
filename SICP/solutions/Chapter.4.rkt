#lang racket
(require "include/interpreter.rkt")

; "eval" is redefined in this file,
; so the "driver-loop" needs to be redefined, too.
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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
  (hash-ref *op-table* (list op type) false))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (car exp)) ((get 'eval (car exp)) exp env))
        ((application? exp) (~apply (eval (operator exp) env)
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
     (make-lambda args (let-body exp))
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

; ========== E4.12
(define (search-env var env recursive action)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (if recursive
                 (env-loop (enclosing-environment env))
                 (action vars vals)))
            ((eq? var (car vars)) (action vars vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (action '() '())
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (lookup-variable-value-4.12 var env)
  (search-env var env true
              (lambda (vars vals)
                (if (null? vars)
                    (error "Unbound variable" var)
                    (mcar vals)))))
(define (set-variable-value-4.12! var val env)
  (search-env var env true
              (lambda (vars vals)
                (if (null? vars)
                    (error "Unbound variable: SET!" var)
                    (set-mcar! vals val)))))
(define (define-variable! var val env)
  (search-env var env false
              (lambda (vars vals)
                (if (null? vars)
                    (add-binding-to-frame! var val (first-frame env))
                    (set-mcar! vals val)))))
