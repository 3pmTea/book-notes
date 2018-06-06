#lang racket

(require racket/mpair)
(require "interpreter.rkt")

(provide (all-defined-out))

(define (lazy-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (lazy-eval-assignment exp env))
        ((definition? exp) (lazy-eval-definition exp env))
        ((if? exp) (lazy-eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (lazy-eval-sequence (begin-actions exp) env))
        ((cond? exp) (lazy-eval (cond->if exp) env))
        ((application? exp)
         (lazy-apply (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (actual-value exp env)
  (force-it (lazy-eval exp env)))

(define (lazy-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (lazy-eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY"
                     procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (mcons (delay-it (first-operand exps)
                       env)
             (list-of-delayed-args (rest-operands exps)
                                   env))))

(define (lazy-eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (lazy-eval (if-consequent exp) env)
      (lazy-eval (if-alternative exp) env)))

(define (lazy-eval-sequence exps env)
  (cond ((last-exp? exps)
         (lazy-eval (first-exp exps) env))
        (else
         (lazy-eval (first-exp exps) env)
         (lazy-eval-sequence (rest-exps exps) env))))

(define (lazy-eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (lazy-eval (assignment-value exp) env)
                       env)
  'ok)

(define (lazy-eval-definition exp env)
  (define-variable! (definition-variable exp)
    (lazy-eval (definition-value exp) env)
    env)
  'ok)

(define lazy-input-prompt ";;; L-Eval input:")
(define lazy-output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input lazy-input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value
            input the-global-environment)))
      (announce-output lazy-output-prompt)
      (user-print output)))
  (driver-loop))

; thunk
(define (force-it-simple obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (tagged-mlist? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))

(define (delay-it exp env)
  (mlist 'thunk exp env))
(define (thunk? obj)
  (tagged-mlist? obj 'thunk))
(define (thunk-exp thunk) (mcar (mcdr thunk)))
(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))

(define (evaluated-thunk? obj)
  (tagged-mlist? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (mcar (mcdr evaluated-thunk)))

(define (force-it-memo obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-mcar! obj 'evaluated-thunk)
           (set-mcar! (mcdr obj) result)
           (set-mcdr! (mcdr obj) '())
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(define force-it force-it-memo)
