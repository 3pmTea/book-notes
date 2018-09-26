#lang racket
(require racket/trace)
(require racket/mpair)
(require "include/interpreter.rkt")

; ========== E4.31
(define (normal-parameter? param) (symbol? param))
(define (lazy-parameter? param)
  (and (pair? param) (eq? 'lazy (cadr param))))
(define (lazy-memo-parameter? param)
  (and (pair? param) (eq? 'lazy-memo (cadr param))))


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
         (~apply (actual-value (operator exp) env)
                 (operands exp)
                 env))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (~apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
         ((compound-procedure? procedure)
          (eval-sequence
           (procedure-body procedure)
           (extend-environment
            (procedure-parameters-extracted procedure)
            (list-of-args (procedure-parameters procedure)
                          arguments
                          env)
            (procedure-environment procedure))))
         (else
          (error "Unknown procedure type: APPLY" procedure))))

(define (procedure-parameters-extracted procedure)
  (map (lambda (p)
         (cond ((normal-parameter? p) p)
               ((lazy-parameter? p) (car p))
               ((lazy-memo-parameter? p) (car p))
               (else (error "Unknown parameter modifier: " p))))
       (procedure-parameters procedure)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-args parameters exps env)
  (if (no-operands? exps)
      '()
      (let ((param (car parameters))
            (exp (car exps)))
        (mcons (cond ((normal-parameter? param) (eval exp env))
                     ((lazy-parameter? param) (delay-it exp env))
                     ((lazy-memo-parameter? param) (delay-memo-it exp env))
                     (else (error "Unknown parameter modifier: " param)))
               (list-of-args (cdr parameters) (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
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

; lazy & lazy-memo
(define (tagged-mlist? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))

(define (delay-it exp env)
  (mlist 'thunk exp env))
(define (delay-memo-it exp env)
  (mlist 'thunk-memo exp env))
(define (thunk? obj)
  (tagged-mlist? obj 'thunk))
(define (thunk-memo? obj)
  (tagged-mlist? obj 'thunk-memo))
(define (thunk-exp thunk) (mcar (mcdr thunk)))
(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))

(define (evaluated-thunk? obj)
  (tagged-mlist? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (mcar (mcdr evaluated-thunk)))

;; 
(define (force-it obj)
  (cond ((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
        ((thunk-memo? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-mcar! obj 'evaluated-thunk)
           (set-mcar! (mcdr obj) result)
           (set-mcdr! (mcdr obj) '())
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))


;
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value
            input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; The extension made to Scheme language in this file can be tested with the example introduced in
;; Exercise 4.27, by attaching the modifiers "lazy" or "lazy-memo" to the parameter "x" of procedure
;; "id".
