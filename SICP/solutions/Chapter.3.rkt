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

; ==========E3.25
; The table structure is similar with Fig. 3.23,
; except that, in order to support keys of arbitrary dimensions,
; the key cells are stored as (mcons KEY VALUE) instead of a single KEY,
; where VALUE is the corresponding value of a key sequence ending with KEY.

(define (make-table-3.25)
  (let ((table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((eq? key (mcar (mcar (mcar records))))
             (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup-rec keys subtable)
      (cond ((null? subtable) false)
            ((null? keys) (mcdr (mcar subtable)))
            (else
             (let ((record (assoc (car keys) (mcdr subtable))))
               (if record
                   (lookup-rec (cdr keys) record)
                   false)))))
    (define (lookup keys)
      (if (null? keys)
          (error "Keys should not be empty")
          (lookup-rec keys table)))
    (define (insert-rec! subtable keys value)
      (if (null? keys)
          (set-mcdr! (mcar (mcar subtable)) value)
          (let ((record (assoc (car keys) (mcdr subtable))))
            (if record
                (insert-rec! record (cdr keys) value)
                (begin
                  (set-mcdr! subtable
                             (mcons (mlist (mcons (car keys) false))
                                    (mcdr subtable)))
                  (insert-rec! (mcdr subtable) (cdr keys) value))))))
    (define (insert! keys value)
      (if (null? keys)
          (error "Keys should not be empty")
          (insert-rec! table keys value)))
    (define (debug-print)
      (display table))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'debug-print) debug-print)
            (else (error "Unknown message" m))))
    dispatch))

; ========== digital circuits simulator
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedures)
  ((wire 'add-action!) action-procedures))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and x y)
  (cond ((and (= x 0) (= y 0)) 0)
        ((and (= x 0) (= y 1)) 0)
        ((and (= x 1) (= y 0)) 0)
        ((and (= x 1) (= y 1)) 1)
        (else (error "Invalid signal" x y))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define (make-time-segment time queue)
  (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))
(define (make-agenda) (mlist 0))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr! segments
                         (mcons (make-new-time-segment time action)
                                (mcdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (mcons (make-new-time-segment time action)
                              segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue? q)
          (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; ========== test the library
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define sum (make-wire))
; (define carry (make-wire))
; (probe 'sum sum)
; (probe 'carry carry)
; (half-adder input-1 input-2 sum carry)
; (set-signal! input-1 1)
; (propagate)

; ========== E3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or x y)
  (cond ((and (= x 0) (= y 0)) 0)
        ((and (= x 0) (= y 1)) 1)
        ((and (= x 1) (= y 0)) 1)
        ((and (= x 1) (= y 1)) 1)
        (else (error "Invalid signal" x y))))

; ========== E3.29
; x && y = !(!x || !y)
(define (or-gate-3.29 a1 a2 output)
  (let ((na1 (make-wire))
        (na2 (make-wire))
        (nout (make-wire)))
    (inverter a1 na1)
    (inverter a2 na2)
    (and-gate na1 na2 nout)
    (inverter nout output)
    'ok))

; test
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define output (make-wire))
; (probe 'output output)
; (or-gate-3.29 input-1 input-2 output)
; (propagate)
; (set-signal! input-1 1)

; ========== E3.30
; delay = N * delay-full-adder
;       = N * (delay-or-gate + 2 * delay-half-adder)

(define (ripple-carry-adder a-list b-list s-list c)
  (let ((c-list (map (lambda (x) (make-wire)) (cdr a-list)))
        (c-0 (make-wire)))
    (map full-adder
         a-list
         b-list
         (append c-list (list c-0))
         s-list
         (cons c c-list))
    'ok))
