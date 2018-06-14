#lang racket

(require "include/vm.rkt")

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
