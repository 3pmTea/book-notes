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

(define (assoc key records)
  (cond ((null? records) false)
        ((eq? key (mcar (car records))) (car records))
        (else (assoc key (cdr records)))))

; queue
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (mcdr (front-ptr queue)))
              queue)))

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

; ========== E3.21
(define (print-queue queue)
  (display (mcar queue)))

; ========== E3.22
(define (make-queue-3.22)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch)
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else (set! front-ptr (mcdr front-ptr)) dispatch)))
    (define (print-queue) (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty-queue?)
            ((eq? m 'front) front-queue)
            ((eq? m 'insert) insert-queue!)
            ((eq? m 'delete) delete-queue!)
            ((eq? m 'print) print-queue)
            (else (error "Unknown message passed to queue"))))
    dispatch))

; ========== E3.23
; deque node structure: (mlist data prev next)
; or equivalently (mcons data (mcons prev next))

; util - node constructor
(define (make-deque-node item) (mlist item '() '()))

; util - node selectors
(define (node-pointers node) (mcdr node))
(define (node-data node) (mcar node))
(define (next-node node) (mcdr (node-pointers node)))
(define (prev-node node) (mcar (node-pointers node)))

; util - node mutators
(define (set-prev-node! node prev-node)
  (set-mcar! (node-pointers node) prev-node))
(define (set-next-node! node next-node)
  (set-mcdr! (node-pointers node) next-node))

; util - deque mutators
(define (set-front-node! deque node)
  (set-mcar! deque node))
(define (set-rear-node! deque node)
  (set-mcdr! deque node))

; util - deque selectors
(define (front-node q) (mcar q))
(define (rear-node q) (mcdr q))

; constructor
(define (make-deque) (mcons '() '()))

; selectors
(define (front-deque q)
  (if (empty-deque? q)
      (error "Can't access front of an empty deque")
      (mcar (front-node q))))
(define (rear-deque q)
  (if (empty-deque? q)
      (error "Can't access rear of an empty deque")
      (mcar (rear-node q))))

; predicate
(define (empty-deque? q)
  (and (eq? (front-node q) '())
       (eq? (rear-node q) '())))

; iterator
(define (print-deque q)
  (define (iter node)
    (cond ((eq? node (rear-node q))
           (display (node-data node)))
          (else
           (display (node-data node))
           (display " ")
           (iter (next-node node)))))
  (display "{")
  (cond ((empty-deque? q) )
        (else
         (iter (front-node q))))
  (display "}"))

; mutators
(define (front-insert-deque! q item)
  (let ((new-node (make-deque-node item)))
    (cond ((empty-deque? q)
           (set-front-node! q new-node)
           (set-rear-node! q new-node)
           q)
          (else
           (set-prev-node! (front-node q) new-node)
           (set-next-node! new-node (front-node q))
           (set-front-node! q new-node)
           q))))

(define (rear-insert-deque! q item)
  (let ((new-node (make-deque-node item)))
    (cond ((empty-deque? q)
           (set-front-node! q new-node)
           (set-rear-node! q new-node)
           q)
          (else
           (set-prev-node! new-node (rear-node q))
           (set-next-node! (rear-node q) new-node)
           (set-rear-node! q new-node)
           q))))

(define (front-delete-deque! q)
  (if (empty-deque? q)
      (error "Can't delete from an empty deque")
      (let ((front (front-node q)))
        (cond ((eq? (rear-node q) front)
               (set-front-node! q '())
               (set-rear-node! q '())
               q)
              (else
               (set-front-node! q (next-node front))
               q)))))

(define (rear-delete-deque! q)
  (if (empty-deque? q)
      (error "Can't delete from an empty deque")
      (let ((rear (rear-node q)))
        (cond ((eq? (front-node q) rear)
               (set-front-node! q '())
               (set-rear-node! q '())
               q)
              (else
               (set-rear-node! q (prev-node rear))
               q)))))

; ==========E3.24
(define (make-table-3.24 same-key?)
  (let ((table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (car records))) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (mcdr table))))
        (if record
            (mcdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (mcdr table))))
        (if record
            (set-mcdr! record value)
            (set-mcdr! table (cons (mcons key value)
                                   (mcdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation on table" m))))
    dispatch))
