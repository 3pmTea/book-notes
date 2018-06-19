#lang racket
(require "include/interpreter.rkt")
(require "include/analyzer.rkt")
(require racket/mpair)

; dispatching utils
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) false))


; "eval" is redefined with data-directed style in this file,
; so the "driver-loop", "~apply" and "eval-sequence" needs to be redefined, too.
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (~apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list->mlist arguments)
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; redefine "analyze" with data-directed style to make it extensible
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((get 'analyze (car exp)) ((get 'analyze (car exp)) exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(put 'analyze 'quote analyze-quoted)
(put 'analyze 'set! analyze-assignment)
(put 'analyze 'define analyze-definition)
(put 'analyze 'if analyze-if)
(put 'analyze 'lambda analyze-lambda)
(put 'analyze 'begin (lambda (exp)
                       (analyze-sequence (begin-actions exp))))
(put 'analyze 'cond (lambda (exp)
                      (analyze (cond->if exp))))

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

; ========== E4.16
; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (let ((value (env-loop env)))
    (if (eq? value '*unassigned*)
        (error "Variable unassigned")
        value)))

; b
(define (make-assignment var val) (cons 'set! (list var val)))

(define (scan-out-defines body-in)
  (let ((defines (filter definition? body-in)))
    (if (null? defines)
        body-in
        (let ((inits (map (lambda (def)
                            (list (definition-variable def) ''*unassigned))
                          defines))
              (assigns (map (lambda (def)
                              (make-assignment
                               (definition-variable def)
                               (definition-value def)))
                            defines))
              (body (filter (lambda (x)
                              (not (definition? x)))
                            body-in)))
          (list (make-let inits (append assigns body)))))))

; c
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; ========== E4.20
(define (letrec->let exp)
  (let ((bindings (let-args exp)))
    (make-let (map (lambda (def)
                     (list (let-var def) ''*unassigned*))
                   bindings)
              (append (map (lambda (def)
                             (make-assignment (let-var def)
                                              (let-exp def)))
                           bindings)
                      (let-body exp)))))

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))
(put 'eval 'letrec eval-letrec)

; ========== E4.21
; a
(define fib-4.21
  (lambda (n)
    ((lambda (fib) (fib fib n))
     (lambda (f k) (if (or (= k 0) (= k 1))
                       1
                       (+ (f f (- k 1))
                          (f f (- k 2))))))))

; b
(define (f-4.21 x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

; ========== E4.22
(define (analyze-let exp)
  (analyze (let->combination exp)))
(put 'analyze 'let analyze-let)
