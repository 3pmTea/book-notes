#lang racket

(require racket/mpair)
(require "include/vm.rkt")

; ==========E5.1
(define (solution-5.1)
  (define vm
    (make-machine
     '(prod counter n)
     (list (list '> >) (list '* *) (list '+ +))
     '((assign prod (const 1))
       (assign counter (const 1))

       fact-loop
       (test (op >) (reg counter) (reg n))
       (branch (label fact-done))
       (assign prod (op *) (reg counter) (reg prod))
       (assign counter (op +) (reg counter) (const 1))
       (goto (label fact-loop))

       fact-done)))
  (let ((n (read)))
    (set-register-contents! vm 'n n)
    (start vm)
    (display (list 'result '= (get-register-contents vm 'prod)))))

; ==========E5.4
(define (solution-5.4a)
  (define vm
    (make-machine
     '(b n val continue)
     (list (list '= =) (list '- -) (list '* *))
     '((assign continue (label expt-done))
       
       expt-begin
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       (save n)
       (save continue)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-expt))
       (goto (label expt-begin))

       after-expt
       (restore continue)
       (restore n)
       (assign val (op *) (reg b) (reg val))
       (goto (reg continue))

       base-case
       (assign val (const 1))
       (goto (reg continue))

       expt-done)))
  (let ((b (begin (display "input base: ") (read)))
        (n (begin (display "input exponent: ") (read))))
    (set-register-contents! vm 'b b)
    (set-register-contents! vm 'n n)
    (start vm)
    (display (list 'result '= (get-register-contents vm 'val)))))

(define (solution-5.4b)
  (define vm
    (make-machine
     '(b n counter prod)
     (list (list '= =) (list '- -) (list '* *))
     '((assign counter (reg n))
       (assign prod (const 1))

       expt-iter
       (test (op =) (reg counter) (const 0))
       (branch (label expt-done))
       (assign counter (op -) (reg counter) (const 1))
       (assign prod (op *) (reg b) (reg prod))
       (goto (label expt-iter))

       expt-done)))
  (let ((b (begin (display "input base: ") (read)))
        (n (begin (display "input exponent: ") (read))))
    (set-register-contents! vm 'b b)
    (set-register-contents! vm 'n n)
    (start vm)
    (display (list 'result '= (get-register-contents vm 'prod)))))

; ==========E5.14
(define (solution-5.14)
  (define vm
    (make-machine
     '()
     (list (list '= =) (list '* *) (list '- -))
     '((assign continue (label fact-done))

       fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))

       after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))

       base-case
       (assign val (const 1))
       (goto (reg continue))

       fact-done)))
  (define (loop)
    (let ((n (begin (display "Input n: ") (read))))
      (set-register-contents! vm 'n n))
    (start vm)
    (display (list 'result '= (get-register-contents vm 'val)))
    (newline)
    (loop))
  (loop))

; ==========E5.20
;           1   2   3    4
; the-car  n1  p1  p1  <free>
; the-cdr  n2  e0  p2  <free>

; ==========E5.21
(define (vm-5.21a)
  (define vm
    (make-machine
     '()
     (list (list 'car car) (list 'cdr cdr) (list 'not not)
           (list 'null? null?) (list 'pair? pair?) (list '+ +))
     '((assign continue (label count-done))
       
       count-leaves
       (test (op null?) (reg t))
       (branch (label count-null))
       (test (op pair?) (reg t))
       (branch (label count-1))
       (goto (label count-leave))

       count-1
       (save t)
       (assign t (op car) (reg t))
       (save continue)
       (assign continue (label count-2))
       (goto (label count-leaves))

       count-2
       (restore continue)
       (restore t)
       (assign t (op cdr) (reg t))
       (save val)
       (save continue)
       (assign continue (label add))
       (goto (label count-leaves))

       add
       (restore continue)
       (assign t (reg val))
       (restore val)
       (assign val (op +) (reg val) (reg t))
       (goto (reg continue))

       count-leave
       (assign val (const 1))
       (goto (reg continue))
                
       count-null
       (assign val (const 0))
       (goto (reg continue))

       count-done)))
  (let ((t (begin (display "input a tree: ") (read))))
    (set-register-contents! vm 't t)
    ; (instruction-trace-on vm)
    ; (register-trace-on vm 't)
    ; (register-trace-on vm 'val)
    (start vm)
    (display (list 'result '= (get-register-contents vm 'val)))))

(define (vm-5.21b)
  (define vm
    (make-machine
     '()
     (list (list 'null? null?) (list 'pair? pair?) (list 'car car)
           (list 'cdr cdr) (list '+ +))
     '((assign n (const 0))
       (assign continue (label count-done))

       count-iter
       (test (op null?) (reg t))
       (branch (label count-null))
       (test (op pair?) (reg t))
       (branch (label count-tree-1))
       (goto (label count-leave))

       count-tree-1
       (save t)
       (assign t (op car) (reg t))
       (save continue)
       (assign continue (label count-tree-2))
       (goto (label count-iter))

       count-tree-2
       (restore continue)
       (restore t)
       (save n)
       (assign n (reg val))
       (assign t (op cdr) (reg t))
       (goto (label count-iter))

       count-null
       (assign val (reg n))
       (goto (reg continue))

       count-leave
       (assign val (op +) (reg n) (const 1))
       (goto (reg continue))

       count-done)))
  (let ((t (begin (display "input a tree: ") (read))))
    (set-register-contents! vm 't t)
    (start vm)
    (display (list 'result '= (get-register-contents vm 'val)))))

; ==========E5.22
(define (vm-5.22a)
  (define vm
    (make-machine
     '()
     (list (list 'null? null?) (list 'car car) (list 'cdr cdr)
           (list 'cons cons))
     '((assign continue (label append-done))

       append
       (test (op null?) (reg x))
       (branch (label null-x))
       (save x)
       (assign x (op cdr) (reg x))
       (save continue)
       (assign continue (label merging))
       (goto (label append))

       merging
       (restore continue)
       (restore x)
       (assign y (op car) (reg x))
       (assign val (op cons) (reg y) (reg val))
       (goto (reg continue))

       null-x
       (assign val (reg y))
       (goto (reg continue))

       append-done)))
  (let ((list1 (begin (display "Input list #1: ") (read)))
        (list2 (begin (display "Input list #2: ") (read))))
    (set-register-contents! vm 'x list1)
    (set-register-contents! vm 'y list2)
    (start vm)
    (display (list 'result '= (get-register-contents vm 'val)))))

(define (vm-5.22b)
  (define vm
    (make-machine
     '()
     (list (list 'cdr mcdr) (list 'null? null?)
           (list 'set-cdr! set-mcdr!))
     '((save x)
       
       last-pair
       (assign val (op cdr) (reg x))
       (test (op null?) (reg val))
       (branch (label base-case))
       (assign x (op cdr) (reg x))
       (goto (label last-pair))

       base-case
       (assign val (reg x))
       
       (perform (op set-cdr!) (reg val) (reg y))
       (restore x))))
  (let ((list1 (begin (display "Input list #1: ") (read)))
        (list2 (begin (display "Input list #2: ") (read))))
    (set-register-contents! vm 'x (list->mlist list1))
    (set-register-contents! vm 'y (list->mlist list2))
    (start vm)
    (display (list 'result '= (mlist->list (get-register-contents vm 'x))))))