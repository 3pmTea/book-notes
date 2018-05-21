#lang racket

(require racket/mpair)

; util procedures

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

; ========== E3.1
(define (make-accumulator-3.1 sum)
  (lambda (value)
    (set! sum (+ sum value))
    sum))

; ========== E3.2
(define (make-monitored-3.2 f)
  (define count 0)
  (define (how-many-calls?) count)
  (define (reset-count)
    (set! count 0))
  (lambda (param)
    (cond ((eq? param 'how-many-calls?) (how-many-calls?))
          ((eq? param 'reset-count) (reset-count))
          (else (set! count (+ count 1))
                (f param)))))

; ========== E3.3
(define (make-account-3.3 balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password x) "Incorrect password")
  (lambda (pw param)
    (cond ((not (eq? pw passwd)) incorrect-password)
          ((eq? param 'withdraw) withdraw)
          ((eq? param 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" param)))))

; ========== E3.4
(define (make-account-3.4 balance passwd)
  (define trials 0)
  (define max-trials 7)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password x) "Incorrect password")
  (define (call-the-cops x) "Called the cops")
  (lambda (pw param)
    (if (eq? pw passwd)
        (begin (set! trials 0)
               (cond ((eq? param 'withdraw) withdraw)
                     ((eq? param 'deposit) deposit)
                     (else (error "Unknown request: MAKE-ACCOUNT" param))))
        (if (< trials max-trials)
            (begin (set! trials (+ trials 1))
                   incorrect-password)
            call-the-cops))))

; ========== E3.5

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (p x y)))
  (let ((ratio (monte-carlo trials experiment))
        (area (abs (* (- y2 y1) (- x2 x1)))))
    (* ratio area)))

(define (pi-3.5)
  (define (in-unit-circle? x y)
    (< (+ (* x x) (* y y)) 1)) 
  (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 100000))

; ========== E3.6
(define (rand-3.5 msg)
  (cond ((eq? msg 'generate) (random))
        ((eq? msg 'reset) (lambda (seed) (random-seed seed)))
        (else (error "Unknown request: RAND-3.5" msg))))

; ========== E3.7
(define (make-account-3.7 balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password x) "Incorrect password")
  (define (make-joint new-password) (dispatch new-password))

  ; This can be further optimized by separating password checking and operation dispatching
  (define (dispatch password)
    (lambda (pw param)
      (cond ((not (eq? pw password)) incorrect-password)
            ((eq? param 'withdraw) withdraw)
            ((eq? param 'deposit) deposit)
            ((eq? param 'make-joint) make-joint)
            (else (error "Unknown request: MAKE-ACCOUNT" param)))))
  (dispatch passwd))

(define (make-joint-3.7 account old-pass new-pass)
  ((account old-pass 'make-joint) new-pass))

;;; test
; (define peter-acc (make-account-3.7 100 'peter))
; (define paul-acc (make-joint-3.7 peter-acc 'peter 'paul))
; ((peter-acc 'paul 'withdraw) 10) -> "Incorrect password"
; ((peter-acc 'peter 'withdraw) 10) -> 90
; ((paul-acc 'paul 'withdraw) 10) -> 80
; ((paul-acc 'peter 'withdraw) 10) -> "Incorrect password"

; ========== E3.8
(define f-3.8
  (let ((called? false))
    (lambda (x)
      (if called?
          0
          (begin (set! called? true)
                 x)))))

;;; test
; (define a (f-3.8 0)) (define b (f-3.8 1)) (+ a b) -> 0
; -- recompile
; (define b (f-3.8 1)) (define a (f-3.8 0)) (+ a b) -> 1

; ========== E3.16
(define (count-mpairs-3.16 x)
  (if (not (mpair? x))
      0
      (+ (count-mpairs-3.16 (mcar x))
         (count-mpairs-3.16 (mcdr x))
         1)))

; (count-mpairs-3.16 pair-3-3.16) -> 3
(define pair-3-3.16
  (mcons 'a (mcons 'b (mcons 'c 'd))))

; (count-mpairs-3.16 pair-4-3.16) -> 4
(define pair-4-3.16
  (let ((pair-a (mcons 'a 'a)))
    (let ((pair-b (mcons pair-a 'b)))
      (let ((pair-c (mcons pair-a pair-b)))
        pair-c))))

; (count-mpairs-3.16 pair-7-3.16) -> 7
(define pair-7-3.16
  (let ((pair-a (mcons 'a 'a)))
    (let ((pair-b (mcons pair-a pair-a)))
      (let ((pair-c (mcons pair-b pair-b)))
        pair-c))))

; (count-mpairs-3.16 pair-inf-3.16) -> infinity
(define pair-inf-3.16
  (let ((pair-a (mcons 'a 'a)))
    (let ((pair-b (mcons 'b pair-a)))
      (let ((pair-c (mcons 'c pair-b)))
        (begin (set-mcdr! pair-a pair-c)
               pair-a)))))

; ========== E3.17

(define (count-mpairs-3.17 x)
  (define visited-pairs (mlist))
  (define (visited? p)
    (define (iter li)
      (cond ((null? li) false)
            ((eq? (mcar li) p) true)
            (else (iter (mcdr li)))))
    (iter visited-pairs))
  (define (count p)
    (cond ((not (mpair? p)) 0)
          ((visited? p) 0)
          (else
           (begin (set! visited-pairs (mcons p visited-pairs))
                  (+ (count (mcar p))
                     (count (mcdr p))
                     1)))))
  (count x))

; ========== E3.18 & E3.19
(define (cycle-list? x)
  (define (iter p1 p2)
    (cond ((null? p1) false)
          ((null? p2) false)
          ((null? (mcdr p2)) false)
          ((eq? p1 p2) true)
          (else (iter (mcdr p1) (mcdr (mcdr p2))))))
  (if (null? x)
      false
      (iter x (mcdr x))))
