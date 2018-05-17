#lang racket

; ==========
; procedures that are defined in the main text

; rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; interval
(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; list operation
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; symbolic differentiation
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; sets operation
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; sorted sets operation
(define (element-of-sortedset? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-sortedset? x (cdr set)))))
(define (intersection-sortedset set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-sortedset (cdr set1)
                                                (cdr set2))))
              ((< x1 x2)
               (intersection-sortedset (cdr set1) set2))
              ((< x2 x1)
               (intersection-sortedset set1 (cdr set2)))))))

; binary tree set
(define (bt-entry tree) (car tree))
(define (bt-left-branch tree) (cadr tree))
(define (bt-right-branch tree) (caddr tree))
(define (bt-make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (bt-left-branch tree)
                      (cons (bt-entry tree)
                            (copy-to-list
                             (bt-right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (bt-make-tree this-entry
                                    left-tree
                                    right-tree)
                      remaining-elts))))))))

; ========== E2.1

(define (make-rat-2.1 n d)
  (define g (gcd n d))
  (let ((num (/ n g))
        (den (/ d g)))
    (cond ((< den 0) (cons (- num) (- den)))
          (else (cons num den)))))

; ========== E2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment segment)
  (define (average x y) (/ (+ x y) 2))
  (let ((x (average (x-point (start-segment segment))
                    (x-point (end-segment segment))))
        (y (average (y-point (start-segment segment))
                    (y-point (end-segment segment)))))
    (make-point x y)))

; ========== E2.3

; Assume that rectangles are axis-aligned

; version 1
(define (make-rect-v1 top-left bottom-right)
  (cons top-left bottom-right))

(define (rect-top-left-v1 rect)
  (car rect))

(define (rect-bottom-right-v1 rect)
  (cdr rect))

(define (distance-x p1 p2)
  (abs (- (x-point p1) (x-point p2))))

(define (distance-y p1 p2)
  (abs (- (y-point p1) (y-point p2))))

(define (length-rect-v1 rect)
  (distance-x (rect-top-left-v1 rect)
              (rect-bottom-right-v1 rect)))

(define (width-rect-v1 rect)
  (distance-y (rect-top-left-v1 rect)
              (rect-bottom-right-v1 rect)))

(define (perimeter-rect-v1 rect)
  (* 2 (+ (width-rect-v1 rect) (length-rect-v1 rect))))

(define (area-rect-v1 rect)
  (* (width-rect-v1 rect) (length-rect-v1 rect)))

; version 2
(define (make-rect-v2 top-left length width)
  (cons top-left (cons length width)))

(define (length-rect-v2 rect)
  (car (cdr rect)))

(define (width-rect-v2 rect)
  (cdr (cdr rect)))

(define (perimeter-rect-v2 rect)
  (* 2 (+ (width-rect-v2 rect) (length-rect-v2 rect))))

(define (area-rect-v2 rect)
  (* (width-rect-v2 rect) (length-rect-v2 rect)))

; ========== E2.4

(define (cons-2.4 x y)
  (lambda (m) (m x y)))

(define (car-2.4 z)
  (z (lambda (p q) p)))

(define (cdr-2.4 z)
  (z (lambda (p q) q)))

; ========== E2.5

(define (cons-2.5 x y)
  (define (exp-iter b e result)
    (cond ((= 0 e) result)
          ((even? e) (exp-iter (* b b) (/ e 2) result))
          (else (exp-iter b (- e 1) (* b result)))))
  (* (exp-iter 2 x 1) (exp-iter 3 y 1)))

(define (iter-2.5 n q i)
  (if (> (remainder n q) 0)
      i
      (iter-2.5 (/ n q) q (+ i 1))))

(define (car-2.5 z)
  (iter-2.5 z 2 0))

(define (cdr-2.5 z)
  (iter-2.5 z 3 0))

; ========== E2.6

(define zero-2.6 (lambda (f) (lambda (x) x)))
(define (add-1-2.6 n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define (one-2.6) (lambda (f) (lambda (x) (f x))))
(define (two-2.6) (lambda (f) (lambda (x) (f (f x)))))

(define (add-2.6 a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; ========== E2.7

(define (upper-bound interval)
  (let ((a (car interval))
        (b (cdr interval)))
    (if (> a b) a b)))

(define (lower-bound interval)
  (let ((a (car interval))
        (b (cdr interval)))
    (if (< a b) a b)))

; ========== E2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; ========== E2.10

(define (div-interval-2.10 x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "Divide by interval that spans 0")
      (div-interval x y)))

; ========== E2.11

(define (mul-interval-2.11 x y)
  (define (pos? t)
    (> (lower-bound t) 0))
  (define (neg? t)
    (< (upper-bound t) 0))
  (define (cross-0? t)
    (and (<= (lower-bound t) 0)
         (>= (upper-bound t) 0)))
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((and (pos? x) (pos? y)) (make-interval (* xl yl) (* xu yu)))
          ((and (pos? x) (cross-0? y)) (make-interval (* xu yl) (* xu yu)))
          ((and (pos? x) (neg? y)) (make-interval (* xu yl) (* xl yu)))
          ((and (cross-0? x) (pos? y)) (make-interval (* xl yu) (* xu yu)))
          ((and (cross-0? x) (cross-0? y)) (let ((p1 (* xl yl))
                                                 (p2 (* xu yu))
                                                 (p3 (* xl yu))
                                                 (p4 (* xu yl)))
                                             (make-interval
                                              (if (< p3 p4) p3 p4)
                                              (if (> p1 p2) p1 p2))))
          ((and (cross-0? x) (neg? y)) (make-interval (* xu yl) (* xl yl)))
          ((and (neg? x) (pos? y)) (make-interval (* xl yu) (* xu yl)))
          ((and (neg? x) (cross-0? y)) (make-interval (* xl yu) (* xl yl)))
          ((and (neg? x) (neg? y)) (make-interval (* xu yu) (* xl yl))))))

; ========== E2.12

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))

(define (percent i)
  (/ (width i) (center i)))

; ========== E2.13

; let X = (make-center-percent x p) and Y = (make-center-percent y q), Z = X * Y, then
; the lower-bound of Z = (x - px)(y - qy) = (1 - p - q + pq)xy,
; the upper-bound of Z = (x + px)(y + qy) = (1 + p + q + pq)xy.
; Since p and q are small, pq can be ignored. Thus (percent Z) = p + q.

; ========== E2.17

(define (last-pair-2.17 l)
  (let ((following (cdr l)))
    (if (null? following)
        (list (car l))
        (last-pair-2.17 following))))

; ========== E2.18

(define (reverse-2.18 l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

; ========== E2.19

(define (cc-2.19 amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc-2.19 amount
                     (except-first-denomination coin-values))
                 (cc-2.19 (- amount (first-denomination coin-values))
                     coin-values)))))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

; ========== E2.20

(define (same-parity x . l)
  (define parity-filter
    (if (even? x) even? odd?))
  (define (iter result remaining)
    (if (null? remaining)
        result
        (let ((e (car remaining))
              (others (cdr remaining)))
          (if (parity-filter e)
              ; this can be done more efficiently
              (iter (append result (list e)) others)
              (iter result others)))))
  (iter (list x) l))

; ========== E2.21

(define (square-list-2.21-1 items)
  (if (null? items)
      '()
      (cons (let ((e (car items)))
              (* e e))
            (square-list-2.21-1 (cdr items)))))

(define (square-list-2.21-2 items)
  (map (lambda (x) (* x x)) items))

; ========== E2.23

(define (for-each-2.23 do-something items)
  (if (null? items)
      (newline)
      ((lambda (l)
        (do-something (car l))
        (for-each-2.23 do-something (cdr l))) items)))

; ========== E2.24

; (1 (2 (3 4)))
;
; OO--OX
; |   |
; 1   OO--OX
;     |   |
;     2   OO--OX
;         |   |
;         3   4
;
;     (1 (2 (3 4)
;          /\
;         /  \
;        1  (2 (3 4)
;              /\
;             /  \
;            2   (3 4)
;                  /\
;                 /  \
;                3    4

; ========== E2.25
; (define a (list 1 3 (list 5 7) 9))
; (define b (list (list 7)))
; (define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;
; (car (cdr (car (cdr (cdr a)))))
; (car (car b))
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

; ========== E2.27

(define (deep-reverse l)
  (define (iter result rest)
    (cond ((null? rest) result)
          ((not (pair? rest)) rest)
          (else (iter (cons (iter (list) (car rest)) result) (cdr rest)))))
  (if (pair? l)
      (iter (list) l)
      (error "expect a list")))

; ========== E2.28

(define (fringe t)
  (define (f tree)
    (cond ((null? tree) tree)
          ((not (pair? tree)) (list tree))
          (else (append (f (car tree)) (f (cdr tree))))))
  (if (pair? t)
      (f t)
      (error "expect a tree")))

; ========== E2.29

(define (make-mobile left right)
  (list left right))
; structure is either weight or mobile
(define (make-branch length structure)
  (list length structure))

; a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

; b
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

; c
(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (cond ((null? mobile) true)
        ((not (pair? mobile)) true)
        (else (let ((l (left-branch mobile))
                    (r (right-branch mobile)))
                (and (= (torque l) (torque r))
                     (balanced? (branch-structure l))
                     (balanced? (branch-structure r)))))))

; d
(define (right-branch-c mobile)
  (cdr mobile))
(define (branch-structure-c branch)
  (cdr branch))

; ========== E2.30

(define (square-tree-1 tree)
  (define (square x) (* x x))
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (t)
         (if (pair? t)
             (square-tree-2 t)
             (* t t)))
       tree))

; ========== E2.31

(define (tree-map f tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

; ========== E2.32

(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x)
                       (cons (car s) x))
                     rest)))))

; ========== E2.33

(define (map-2.33 p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              sequence))
(define (append-2.33 seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-2.33 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; ========== E2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; ========== E2.35

; I use a recursion here
(define (count-leaves-2.35 t)
  (accumulate +
              0
              (map (lambda (x)
                     (cond ((null? x) 0)
                           ((pair? x)
                            (count-leaves-2.35 x))
                           (else 1))) t)))

; ========== E2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; ========== E2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

; ========== E2.39

(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-2.39-r sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              '()
              sequence))
(define (reverse-2.39-l sequence)
  (fold-left (lambda (x y)
               (cons y x))
             '()
             sequence))

; ========== E2.40

; helper functions
(define (enumerate-interval x y)
  (if (> x y)
      '()
      (cons x (enumerate-interval (+ 1 x) y))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (prime? n)
  (define (iter i n)
    (cond ((> (* i i) n) true)
          ((> (gcd i n) 1) false)
          (else (iter (+ 1 i) n))))
  (iter 2 n))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
         (map (lambda (j)
                (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; ========== E2.41

; Solution 1:
; Generate all triples (i, j, k), where 1 <= i < j < k <= n,
; filter the triples satisfying i + j + k = s.
; This solution makes C(n, 3) triples.

(define (sum-to s)
  (lambda (triple)
    (= s (+ (car triple)
            (cadr triple)
            (caddr triple)))))

(define (list-triples-2.41 n)
  (flatmap (lambda (i)
         (flatmap (lambda (j)
                (map (lambda (k)
                       (list i j k))
                     (enumerate-interval (+ 1 j) n)))
                (enumerate-interval (+ 1 i) n)))
       (enumerate-interval 1 n)))

(define (solution-2.41-1 n s)
  (filter (sum-to s) (list-triples-2.41 n)))

; Solution 2:
; Generate all triples (i, j, s-i-j), where 1 <= i < j <= n,
; filter the pairs satisfying j < s-i-j <= n.
; This solution makes C(n, 2) triples.

(define (solution-2.41-2 n s)
  (define (k-filter triple)
    (let ((j (cadr triple))
          (k (caddr triple)))
      (and (< j k) (<= k n))))
  (define (list-triples)
    (flatmap (lambda (i)
               (map (lambda (j)
                      (list i j (- (- s i) j)))
                    (enumerate-interval (+ 1 i) n)))
             (enumerate-interval 1 n)))
  (filter k-filter (list-triples)))

; ========== E2.42

; First we determine the form of the result. It may be:
; Result = list of Solutions
; a Solution = list of Coordinates
; Coordinate = (row column)
;
; So the output would be (((row1 col1) (row2 col2) ...) ...other solutions)
(define (queens-2.42 n)
  (define empty-board '())
  (define (adjoin-position row col rest)
    (cons (list row col) rest))

  ; I didn't use k in this filter
  (define (safe? k positions)
    (let ((trial (car positions))
          (trial-row (caar positions))
          (trial-col (cadar positions))
          (rest (cdr positions)))
      (accumulate (lambda (pos result)
                    (let ((row (car pos))
                          (col (cadr pos)))
                      (and (not (= (- trial-row trial-col)
                                   (- row col)))
                           (not (= (+ trial-row trial-col)
                                   (+ row col)))
                           (not (= trial-row row))
                           result)))
                  true
                  rest)))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions)
                  (safe? k positions))
                (flatmap (lambda (rest)
                           (map (lambda (new-row)
                                  (adjoin-position
                                   new-row k rest))
                                (enumerate-interval 1 n)))
                         (queen-cols (- k 1))))))
  (queen-cols n))

; ========== E2.54
(define (equal?-2.54 a b)
  (if (pair? a)
      (and (pair? b)
           (equal?-2.54 (car a) (car b))
           (equal?-2.54 (cdr a) (cdr b)))
      (and (not (pair? b)) (eq? a b))))

; ========== E2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (deriv (base exp) var))
                       (make-exponentiation (base exp)
                                            (make-sum (exponent exp) -1))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (make-exponentiation base exp)
  (cond ((not (number? exp)) (error "exponent should be a number"))
        ((= exp 0) 1)
        ((= exp 1) base)
        ((number? base) (expt base exp))
        (else (list '** base exp))))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

; ========== E2.57
(define (make-sum a1 a2 . rest-terms)
  (define (iter constant vars terms)
    (if (null? terms)
        (if (= constant 0)
            (cond ((null? vars) 0)
                  ((null? (cdr vars)) (car vars))
                  (else (cons '+ vars)))
            (cond ((null? vars) constant)
                  ((null? (cdr vars)) (cons '+ (cons constant vars)))
                  (else (cons '+ (cons constant vars)))))
        (let ((term (car terms))
              (rest (cdr terms)))
          (if (number? term)
              (iter (+ term constant) vars rest)
              (iter constant (cons term vars) rest)))))
  (iter 0 '() (cons a1 (cons a2 rest-terms))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2 . rest-factors)
  (define (iter constant vars factors)
    (if (= constant 0)
        0
        (if (null? factors)
            (if (= constant 1)
                (cond ((null? vars) 1)
                      ((null? (cdr vars)) (car vars))
                      (else (cons '* vars)))
                (cond ((null? vars) constant)
                      ((null? (cdr vars)) (cons '* (cons constant vars)))
                      (else (cons '* (cons constant vars)))))
            (let ((factor (car factors))
                  (rest (cdr factors)))
              (if (number? factor)
                  (iter (* factor constant) vars rest)
                  (iter constant (cons factor vars) rest))))))
  (iter 1 '() (cons m1 (cons m2 rest-factors))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (let ((term (caddr s))
        (rest (cdddr s)))
    (if (null? rest)
        term
        (cons '+ (cons term rest)))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (let ((factor (caddr p))
        (rest (cdddr p)))
    (if (null? rest)
        factor
        (cons '* (cons factor rest)))))

; ========== E2.58
; a
(define (deriv-infix exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum-infix? exp) (make-sum-infix (deriv-infix (addend-infix exp)
                                                       var)
                                          (deriv-infix (augend-infix exp)
                                                       var)))
        ((product-infix? exp)
         (make-sum-infix (make-product-infix (multiplier-infix exp)
                                             (deriv-infix (multiplicand-infix exp)
                                                          var))
                         (make-product-infix (multiplicand-infix exp)
                                             (deriv-infix (multiplier-infix exp)
                                                          var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (sum-infix? exp)
  (and (pair? exp) (pair? (cdr exp)) (eq? (cadr exp) '+)))
(define (make-sum-infix a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list a '+ b))))
(define (addend-infix exp)
  (car exp))
(define (augend-infix exp)
  (caddr exp))

(define (product-infix? exp)
  (and (pair? exp) (pair? (cdr exp)) (eq? (cadr exp) '*)))
(define (make-product-infix a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list a '* b))))
(define (multiplier-infix exp)
  (car exp))
(define (multiplicand-infix exp)
  (caddr exp))

; b

; ========== E2.59
; see http://community.schemewiki.org/?sicp-ex-2.59 for more optimized solutions
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

; ========== E2.60
(define (element-of-multiset? x set)
  (element-of-set? x set))

(define (adjoin-multiset x set)
  (cons x set))

(define (union-multiset set1 set2)
  (append set1 set2))

(define (intersection-multiset set1 set2)
  (intersection-set set1 set2))

; ========== E2.61
(define (adjoin-sortedset x set)
  (cond ((null? set) '(x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-sortedset x (cdr set))))))

; ========== E2.62
(define (union-sortedset set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1)
                                         (union-sortedset (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1)
                                         (union-sortedset (cdr set1) set2)))
        (else (cons (car set2) (union-sortedset set1 (cdr set2))))))

; ========== E2.65
(define (union-btset set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (union-sortedset list1 list2))))

(define (intersection-btset set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (intersection-sortedset list1 list2))))
