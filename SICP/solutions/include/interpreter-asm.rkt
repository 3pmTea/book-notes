#lang racket

(require racket/mpair)
(require "interpreter.rkt")
(require "vm.rkt")

; new util functions
(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))

(define (get-global-environment) the-global-environment)

; E5.23
(define (make-application operator operands) (cons operator operands))
(define (make-assignment var val) (cons 'set! (list var val)))

; let
(define (let? exp) (tagged-list? exp 'let))
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

; let*
(define (let*? exp) (tagged-list? exp 'let*))
(define (make-let args body) (cons 'let (cons args body)))
(define (make-let* args body) (cons 'let* (cons args body)))
(define (let*->nested-lets exp)
  (define (reduce-let* args body)
    (if (null? args)
        (sequence->exp body)
        (make-let (list (car args))
                  (list (reduce-let* (cdr args) body)))))
  (reduce-let* (let-args exp) (let-body exp)))

; named let
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

; letrec
(define (letrec? exp) (tagged-list? exp 'letrec))
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

; cond - this doesn't perform syntax check, assuming the syntax is valid
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clauses)
  (eq? (cond-first-clause-predicate clauses) 'else))
(define (cond-first-clause-predicate clauses) (caar clauses))
(define (cond-first-clause-actions clauses) (cdar clauses))
(define (cond-rest-clauses clauses) (cdr clauses))

; op table of the vm
(define eceval-operations
  (list (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        `(null? ,null?)
        `(lambda-body ,lambda-body)
        `(make-procedure ,make-procedure)
        `(operands ,operands)
        `(operator ,operator)
        `(empty-arglist ,empty-arglist)
        `(no-operands? ,no-operands?)
        `(first-operand ,first-operand)
        `(last-operand? ,last-operand?)
        `(adjoin-arg ,adjoin-arg)
        `(rest-operands ,rest-operands)
        `(primitive-procedure? ,primitive-procedure?)
        `(compound-procedure? ,compound-procedure?)
        `(apply-primitive-procedure ,apply-primitive-procedure)
        `(procedure-parameters ,procedure-parameters)
        `(procedure-environment ,procedure-environment)
        `(extend-environment ,extend-environment)
        `(procedure-body ,procedure-body)
        `(begin-actions ,begin-actions)
        `(first-exp ,first-exp)
        `(last-exp? ,last-exp?)
        `(rest-exps ,rest-exps)
        `(if-predicate ,if-predicate)
        `(true? ,true?)
        `(if-alternative ,if-alternative)
        `(if-consequent ,if-consequent)
        `(assignment-variable ,assignment-variable)
        `(assignment-value ,assignment-value)
        `(set-variable-value! ,set-variable-value!)
        `(definition-variable ,definition-variable)
        `(definition-value ,definition-value)
        `(define-variable! ,define-variable!)
        `(list->mlist ,list->mlist)
        `(let? ,let?)
        `(let*? ,let*?)
        `(letrec? ,letrec?)
        `(let->combination ,let->combination)
        `(let*->nested-lets ,let*->nested-lets)
        `(letrec->let ,letrec->let)
        `(cond? ,cond?)
        `(cond-clauses ,cond-clauses)
        `(cond-else-clause? ,cond-else-clause?)
        `(cond-first-clause-predicate ,cond-first-clause-predicate)
        `(cond-first-clause-actions ,cond-first-clause-actions)
        `(cond-rest-clauses ,cond-rest-clauses)
        `(cond-else-clause? ,cond-else-clause?)))

; the interpreter running on the vm
(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))

     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output) (const ";;EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))

     unknown-procedure-type
     (restore continue)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op cond?) (reg exp)) ; E5.24
     (branch (label ev-cond))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op let?) (reg exp)) ; E5.23
     (branch (label ev-let))
     (test (op let*?) (reg exp)) ; E5.23
     (branch (label ev-let*))
     (test (op letrec?) (reg exp)) ; E5.23
     (branch (label ev-letrec))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))

     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-let ; E5.23
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))

     ev-let* ; E5.23
     (assign exp (op let*->nested-lets) (reg exp))
     (goto (label eval-dispatch))

     ev-letrec ; E5.23
     (assign exp (op letrec->let) (reg exp))
     (goto (label eval-dispatch))

     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev)
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)

     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))

     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val
             (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign argl (op list->mlist) (reg argl)) ; added for racket compatibility
     (assign env (op extend-environment)
             (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))

     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))

     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))

     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-cond ; E5.24
     (assign unev (op cond-clauses) (reg exp))
     (save continue)

     ev-cond-loop
     (test (op null?) (reg unev))
     (branch (label ev-cond-empty-clause))
     ; flaw: it doesn't perform a validation for the usage of 'else'
     (test (op cond-else-clause?) (reg unev))
     (branch (label ev-cond-do-action))
     (assign exp (op cond-first-clause-predicate) (reg unev))
     (save unev)
     (save env)
     (assign continue (label ev-cond-decide))
     (goto (label eval-dispatch))

     ev-cond-decide
     (restore env)
     (restore unev)
     (test (op true?) (reg val))
     (branch (label ev-cond-do-action))
     (assign unev (op cond-rest-clauses) (reg unev))
     (goto (label ev-cond-loop))

     ev-cond-do-action
     (assign unev (op cond-first-clause-actions) (reg unev))
     (goto (label ev-sequence))

     ev-cond-empty-clause
     (assign val (const 'undefined))
     (restore continue)
     (goto (reg continue))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))

     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))

     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue)))))


