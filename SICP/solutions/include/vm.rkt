#lang racket

(require racket/mpair)

(provide
 
 ; basic operations
 make-machine
 set-register-contents!
 get-register-contents
 start

 ; analyzing interfaces
 analyze

 ; debugging interfaces
 print-stack-stat
 reset-stack-stat
 print-execution-count
 reset-execution-count
 instruction-trace-on
 instruction-trace-off
 register-trace-on
 register-trace-off
 set-breakpoint
 cancel-breakpoint
 cancel-all-breakpoints
 proceed-machine)

;; vm public interface definitions
(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))
(define (print-stack-stat machine)
  ((machine 'stack) 'print-statistics))
(define (reset-stack-stat machine)
  ((machine 'stack) 'initialize))
(define (print-execution-count machine)
  (machine 'print-execution-count))
(define (reset-execution-count machine)
  (machine 'reset-execution-count))
(define (instruction-trace-on machine)
  (machine 'trace-on))
(define (instruction-trace-off machine)
  (machine 'trace-off))
(define (register-trace-on machine reg-name)
  (((machine 'get-register) reg-name) 'trace-on))
(define (register-trace-off machine reg-name)
  (((machine 'get-register) reg-name) 'trace-off))
(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))
(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))
(define (proceed-machine machine)
  (machine 'proceed))
(define (analyze machine)
  (machine 'analyze))


;; private
(define (force-get-register machine reg-name)
  ((machine 'force-get-register) reg-name))

;; test vm
(define (test-vm)
  (define gcd-machine
    (make-machine
     '()
     (list (list 'rem remainder) (list '= =))
     '(test-b (test (op =) (reg b) (const 0))
              (branch (label gcd-done))
              (assign t (op rem) (reg a) (reg b))
              (assign a (reg b))
              (assign b (reg t))
              (goto (label test-b))
              gcd-done)))
  (set-register-contents! gcd-machine 'a 206)
  (set-register-contents! gcd-machine 'b 40)
  (start gcd-machine)
  (get-register-contents gcd-machine 'a)) ;; exptected output: 2

;; test debugger
(define (test-debug) 'done)

;; mlist util
(define (mfor-each f l)
  (if (null? l)
      'done
      (begin (f (mcar l))
             (mfor-each f (mcdr l)))))

;; register
(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on false)) ; E5.18
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (when trace-on  ; E5.18
                 (begin
                   (display (list 'register name
                                  'old-value '= contents
                                  'new-value '= value))
                   (newline)))
               (set! contents value)))
            ((eq? message 'trace-on) ; E5.18
             (set! trace-on true))
            ((eq? message 'trace-off) ; E5.18
             (set! trace-on false))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))
(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

;; stack
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth))
      (newline))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else (error "Unknown request: STACK" message))))
    dispatch))
(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;; machine
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (define (execute-only inst) ((instruction-execution-proc inst)))
  
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-labels '())
        (execution-count 0) ; added for E5.15
        (execute-instruction execute-only) ; label tracing
        (breakpoints (make-hash))
        (saved-pc (mlist '())))
    (define (execute-with-trace inst)
      (when (executable-instruction? inst) ; label or breakpoint
        (set! execution-count (+ execution-count 1))) ; E5.15
      (when (not (eq? (instruction-text inst) 'breakpoint))
        (display (instruction-text inst)))
      (newline)
      ((instruction-execution-proc inst)))
    
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (lookup-or-allocate-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (let ((reg (make-register name)))
                (set! register-table
                      (cons (list name reg) register-table))
                (display "Auto allocated new register - ")
                (display name)
                (newline)
                reg))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'halt)
                ((mpair? insts)
                 (execute-instruction (mcar insts))
                 (execute))
                (else insts))))
      (define (set-breakpoint label-text n)
        (let ((label (assoc label-text the-labels)))
          (if label
              (insert-breakpoint breakpoints label n pc saved-pc)
              (error "Unknown label:" label-text))))
      (define (cancel-breakpoint label-text n)
        (let ((label (assoc label-text the-labels)))
          (if label
              (remove-breakpoint breakpoints label-text n)
              (error "Unknown label:" label-text))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (assembly)
                 (set! the-instruction-sequence (car assembly))
                 (set! the-labels (cdr assembly))))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'force-get-register)
               lookup-or-allocate-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'print-execution-count)
               (display (list 'execution-count '= execution-count))) ; E5.15
              ((eq? message 'reset-execution-count)
               (set! execution-count 0)) ; E5.15
              ((eq? message 'trace-on)
               (set! execute-instruction execute-with-trace)) ; E5.16
              ((eq? message 'trace-off)
               (set! execute-instruction execute-only)) ; E5.16
              ((eq? message 'proceed)
               (set-contents! pc (mcar saved-pc))
               (set-mcar! saved-pc '())
               (execute))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints)
               (remove-all-breakpoints breakpoints))
              ((eq? message 'analyze)
               (error "todo"))
              (else (error "Unknown request: MACHINE" message))))
      dispatch)))

;; assembler
(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     (cons insts labels))))
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels) ; E5.8
                   (error "Duplicate label entry name: " next-inst)
                   (receive insts
                            (cons (make-label-entry next-inst insts)
                                  labels)))
               (receive (mcons (make-instruction next-inst)
                               insts)
                        labels)))))))
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (mfor-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
     insts)
    ;; add label information into instructions to support E5.16
    (define (process label-entry)
      (when (not (null? (cdr label-entry)))
        (prepend-label (car label-entry) (cdr label-entry) pc)))
    (define (iter f l)
      (cond ((null? l) 'done)
            ((or (null? (cdr l))
                 (not (eq? (cdar l) (cdadr l))))
             (f (car l))
             (iter f (cdr l)))
            (else (iter f (cdr l)))))
    (iter process labels)))

(define (make-instruction text) (mcons text '()))
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst) (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc))
(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" label-name))))
(define (prepend-label text insts pc)
  (let ((new-insts (mcons (mcar insts) (mcdr insts)))
        (inst (mcar insts))
        (label-inst (make-instruction text)))
    (set-mcdr! label-inst
               (lambda () (advance-pc pc)))
    (set-mcar! insts label-inst)
    (set-mcdr! insts new-insts)))
(define (executable-instruction? inst) ; instructions except labels or breakpoints
  (pair? (instruction-text inst)))

;; instruction dispatcher
(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
         (error "Unknown instruction type: ASSEMBLE" inst))))

;; assign instruction
(define (make-assign inst machine labels operations pc)
  (let ((target
         (force-get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))
(define (advance-pc pc)
  (set-contents! pc (mcdr (get-contents pc))))

;; test instruction
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

;; branch instruction
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;; goto instruction
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label
                         labels
                         (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (force-get-register
                       machine
                       (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; save instruction
(define (make-save inst machine stack pc)
  (let ((reg (force-get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
;; restore instruction
(define (make-restore inst machine stack pc)
  (let ((reg (force-get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;; perform instruction
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (force-get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e) ; E5.9
                    (error "Operations can't be used with labels")
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE" symbol))))

;; debugger
(define (make-breakpoint-instruction label-text n pc saved-pc)
  (mcons 'breakpoint
         (lambda ()
           (set-mcar! saved-pc (mcdr (get-contents pc)))
           (set-contents! pc 'paused)
           (display "Breakpoint reached: ")
           (display (list label-text n))
           (newline))))
(define (insert-breakpoint breakpoints label n pc saved-pc)
  (define (find-instruction insts i)
    (cond ((not (executable-instruction? (mcar insts)))
           (find-instruction (mcdr insts) i))
          ((> i 1)
           (find-instruction (mcdr insts) (- i 1)))
          (else insts)))
  (if (hash-ref breakpoints (cons (car label) n) false)
      (error "Breakpoint exist:" (car label) n)
      (let ((insts (find-instruction (cdr label) n))
            (breakpoint (make-breakpoint-instruction
                         (car label) n pc saved-pc)))
        (let ((new-insts (mcons (mcar insts) (mcdr insts))))
          (set-mcar! insts breakpoint)
          (set-mcdr! insts new-insts)
          (hash-set! breakpoints (cons (car label) n) insts)))))
(define (remove-breakpoint breakpoints label-text n)
  (let ((breakpoint (hash-ref breakpoints (cons label-text n) false)))
    (if breakpoint
        (begin
          (set-mcar! breakpoint (mcar (mcdr breakpoint)))
          (set-mcdr! breakpoint (mcdr (mcdr breakpoint)))
          (hash-remove! breakpoints (cons label-text n))
          (display (list "Removed breakpoint at" label-text n)))
        (error "Breakpoint does not exist:" label-text n))))
(define (remove-all-breakpoints breakpoints)
  (hash-for-each
   breakpoints
   (lambda (key breakpoint)
     (set-mcar! breakpoint (mcar (mcdr breakpoint)))
     (set-mcdr! breakpoint (mcdr (mcdr breakpoint)))))
  (hash-clear! breakpoints)
  (display "Breakpoints cleared"))
                 
  
