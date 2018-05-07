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

