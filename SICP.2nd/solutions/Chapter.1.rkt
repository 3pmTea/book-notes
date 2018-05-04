#lang racket

; ========== E1.3

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (solution-1.3 a b c)
  (cond ((and (<= a b) (<= a c)) (sum-of-squares b c))
        ((and (<= b a) (<= b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))

; ========== E1.7

; Solution-1.7
; Since 'new-if' is not a special form, with applicative-order evaluation performed,
; the recursive 'improve' clause will be evaluated whether or not the guess is good enough.
; Thus leading to stack overflow.

; ========== E1.8

(define (cubic x)
  (* x x x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (cubic guess) x)) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define solution-1.8 cube-root)

; ========== E1.10

; Solution-1.10
; f(n) = A(0, n) = 2n
; g(n) = A(1, n) = A(0, A(1, n-1)) = 2A(1, n-1) = ... = 2^(n-1)A(1, 1) = 2^n
; h(n) = A(2, n) = A(1, A(2, n-1)) = g(A(2, n-1)) = 2^A(2, n-1) = 2^(2^(2^...2^2))

; ========== E1.11

(define (f-rec n)
  (if (< n 3) n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter-impl a b c n)
  (cond ((< n 2) n)
        ((= n 2) c)
        (else (f-iter-impl b c (+ c (* 2 b) (* 3 a)) (- n 1)))))
  
(define (f-iter n)
  (f-iter-impl 0 1 2 n))

(define solution-1.11-rec f-rec)
(define solution-1.11-iter f-iter)

; ========== E1.12

(define (pascal-rec row col)
  (cond ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal-rec (- row 1) (- col 1)) (pascal-rec (- row 1) col)))))

(define solution-1.12 pascal-rec)

; ========== E1.16

(define (fast-exp-iter factor base exponent)
  (cond ((= exponent 1) (* factor base))
        ((even? exponent) (fast-exp-iter factor (square base) (/ exponent 2)))
        (else (fast-exp-iter (* factor base) base (- exponent 1)))))

(define (solution-1.16 b n)
  (fast-exp-iter 1 b n))

; ========== E1.19

; T^2: a <- b(q^2 + 2pq) + a(2q^2 + 2pq + p^2), b <- b(p^2 + q^2) + a(2pq + q^2)
; so P = p^2 + q^2, Q = 2pq + q^2

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; ========== E1.22
(define runtime current-milliseconds)

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (newline)))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes-iter from count)
  (cond ((= 0 count) (newline))
        ((even? from) (search-for-primes-iter (+ 1 from) count))
        ((prime? from) (timed-prime-test from) (search-for-primes-iter (+ 2 from) (- count 1)))
        (else (search-for-primes-iter (+ 2 from) count))))
(define solution-1.22 search-for-primes-iter)

; output:
;> (search-for-primes-iter 100000000000 3)
;100000000003 *** 16
;100000000019 *** 18
;100000000057 *** 16
;
;> (search-for-primes-iter 10000000000000 3)
;10000000000037 *** 185
;10000000000051 *** 187
;10000000000099 *** 179
;
;> (search-for-primes-iter 1000000000000000 3)
;1000000000000037 *** 1750
;1000000000000091 *** 1767
;1000000000000159 *** 1747
;> 
; We can see that if N is large enough, when N gets 100 times larger, it takes 10 times longer to check for its primality.

; ========== E1.27

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (try-from a)
    (cond ((= a n) true)
          ((try-it a) (try-from (+ 1 a)))
          (else false)))
  (try-from 2))

; ========== E1.29

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* h k))))
  (define (term k)
    (cond ((= k 0) (y k))
          ((= k n) (y n))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (define (next k) (+ k 1))
  (/ (* h (sum term 0 next n)) 3))

; ========== E1.30

(define (solution-1.30 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (test-1.30 n)
  (solution-1.30 (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

; ========== E1.31

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-1.31 n)
  (product-iter (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

(define (pi-1.31 n)
  (define (term x)
    (if (even? x)
        (/ (+ 2 x) (+ 1 x))
        (/ (+ 1 x) (+ 2 x))))
  (* 4.0 (product-iter term 1 (lambda (x) (+ 1 x)) n)))

; ========== E1.32

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        (combiner null-value result)
        (iter (next a)
              (combiner (term a) result))))
  (iter a null-value))

(define (sum-1.32 term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-1.32 term a next b)
  (accumulate-iter * 1 term a next b))

; ========== E1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        (combiner null-value result)
        (iter (next a)
              (combiner result (if (filter a)
                                   (term a)
                                   null-value)))))
  (iter a null-value))

(define (solution-1.33-a a b)
  (filtered-accumulate prime? + 0 square a (lambda (x) (+ 1 x)) b))

(define (solution-1.33-b n)
  (filtered-accumulate (lambda (x) (= 1 (gcd x n)))
                       *
                       1
                       (lambda (x) x)
                       2
                       (lambda (x) (+ 1 x))
                       (- n 1)))

; ==========