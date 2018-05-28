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

; ========== E3.65
(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-summands (+ 1 n)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

(define ln2-stream-acc (accelerated-sequence euler-transform ln2-stream))

; ========== E3.67
; solution 1
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (upper-pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (upper-pairs (stream-cdr s) (stream-cdr t)))))

(define (flip-pairs s)
  (stream-map (lambda (x) (list (cadr x) (car x))) s))

(define integer-pairs
  (let ((upper (upper-pairs integers integers)))
    (interleave upper
                (stream-filter (lambda (x) (> (car x) (cadr x))) (flip-pairs upper)))))

; solution 2
; uploaded to http://community.schemewiki.org/?sicp-ex-3.68
(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) t))))

; ========== E3.69
; a not effecient version, which requires 1G RAM to compute only 3 triples (3 4 5) (6 8 10) (5 12 13)
(define (stream-combine make-element s t)
  (stream-cons
   (make-element (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (make-element (stream-car s) x))
                (stream-cdr t))
    (stream-combine make-element (stream-cdr s) t))))

(define (triples s t u)
  (let ((all-triples (stream-combine (lambda (x y) (cons x y))
                                     s
                                     (stream-combine (lambda (x y)
                                                       (list x y))
                                                     t
                                                     u))))
    (stream-filter (lambda (x)
                     (and (< (car x) (cadr x))
                          (< (cadr x) (caddr x))))
                   all-triples)))

(define pythagorean-triples
  (stream-filter (lambda (x) (= (+ (square (car x)) (square (cadr x)))
                                (square (caddr x))))
                 (triples integers integers integers)))

; ========== E3.70
(define (merge-weighted weight s t)
  (cond ((stream-null? s) t)
        ((stream-null? t) s)
        (else
         (let ((ws (weight (stream-car s)))
               (wt (weight (stream-car t))))
           (if (<= ws wt)
               (stream-cons (stream-car s)
                            (merge-weighted weight (stream-cdr s) t))
               (stream-cons (stream-car t)
                            (merge-weighted weight s (stream-cdr t))))))))

(define (weighted-pairs weight s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (weighted-pairs weight (stream-cdr s) t))))

; a
(define pairs-a (stream-filter
                 (lambda (x) (<= (car x) (cadr x)))
                 (weighted-pairs (lambda (x)
                                   (+ (car x) (cadr x)))
                                 integers
                                 integers)))
;b
(define pairs-b
  (stream-filter
   (lambda (x)
     (let ((prod (* (car x) (cadr x))))
       (not (or (> (car x) (cadr x))
                (= (remainder prod 2) 0)
                (= (remainder prod 3) 0)
                (= (remainder prod 5) 0)))))
   (weighted-pairs (lambda (x)
                     (+ (* 2 (car x))
                        (* 3 (cadr x))
                        (* 5 (car x) (cadr x))))
                   integers
                   integers)))
