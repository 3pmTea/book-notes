#lang racket

(define stream-car stream-first)
(define stream-cdr stream-rest)
(define stream-null? stream-empty?)

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons low
                   (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s n)
  (define (iter str i)
    (when (and (< i n)
               (not (stream-null? str)))
          (begin (display (stream-car str))
                 (display " ")
                 (iter (stream-cdr str) (+ i 1)))))
  (display "(")
  (iter s 0)
  (display ")")
  (newline))

(define integers (stream-cons 1 (stream-map add1 integers)))

(define (mul-streams x y) (stream-map-3.50 * x y))

(define (add-streams x y) (stream-map-3.50 + x y))

(define (stream-scale s factor)
  (stream-map (lambda (x) (* x factor)) s))

; ========== E3.50

(define (stream-map-3.50 proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
        (apply proc (map stream-car argstreams))
        (apply stream-map-3.50
               (cons proc (map stream-cdr argstreams))))))

(define (test-3.50)
  (define a (stream 1 3 5))
  (define b (stream 7 8 9))
  (display (stream->list (stream-map-3.50 + a b))))

; ========== E3.54
(define factorials (stream-cons 1 (mul-streams factorials (stream-cdr integers))))

; ========== E3.55
(define (partial-sums s)
  (add-streams s (stream-cons 0 (partial-sums s))))

; ========== E3.56
(define (stream-merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (stream-cons
                   s1car
                   (stream-merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (stream-cons
                   s2car
                   (stream-merge s1 (stream-cdr s2))))
                 (else
                  (stream-cons
                   s1car
                   (stream-merge (stream-cdr s1)
                                 (stream-cdr s2)))))))))

(define s-3.56
  (stream-cons 1 (stream-merge (stream-scale s-3.56 2)
                               (stream-merge (stream-scale s-3.56 3)
                                             (stream-scale s-3.56 5)))))

; ========== E3.59
; a
(define (integrate-series s)
  (mul-streams s (stream-map (lambda (x) (/ 1 x)) integers)))

; b
(define cosine-series (stream-cons 1 (integrate-series
                                      (stream-scale sine-series -1))))
(define sine-series (stream-cons 0 (integrate-series cosine-series)))

; ========== E3.60
(define add-series add-streams)
(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams
                (add-streams
                 (stream-scale (stream-cdr s2) (stream-car s1))
                 (stream-scale (stream-cdr s1) (stream-car s2)))
                (stream-cons 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define (test-3.60)
  (define circle-series
    (add-streams (mul-series sine-series sine-series)
                 (mul-series cosine-series cosine-series)))
  (display-stream circle-series 10))  ; expected: 1 followed by 0s

; ========== E3.61
(define (invert-unit-series s)
  (stream-cons 1 (stream-scale
                  (mul-series (stream-cdr s)
                              (invert-unit-series s))
                  -1)))

; ========== E3.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "denominator has a zero constant term - DIV-SERIES")
      (mul-series s1 (invert-unit-series s2))))

(define tangent-series (div-series sine-series cosine-series))

; ========== E3.64
(define (stream-limit s tolerance)
  (let ((e1 (stream-car s))
        (e2 (stream-car (stream-cdr s))))
    (if (< (abs (- e1 e2)) tolerance)
        e2
        (stream-limit (stream-cdr s) tolerance))))

(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (stream-cons
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)
(define (sqrt-3.64 x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
