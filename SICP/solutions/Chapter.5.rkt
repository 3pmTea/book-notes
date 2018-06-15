#lang racket

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
     '(n val continue)
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
    (let ((n (read)))
      (set-register-contents! vm 'n n))
    (start vm)
    (display (list 'result '= (get-register-contents vm 'val)))
    (newline)
    (loop))
  (set-breakpoint vm 'after-fact 4)
  (instruction-trace-on vm)
  (loop))

; ==========E5.20
;           1   2   3    4
; the-car  n1  p1  p1  <free>
; the-cdr  n2  e0  p2  <free>

