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

(define (cube x)
  (* x x x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

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

; ========== E1.36

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (solution-1.36 guess)
  (define (f x)
    (/ (log 1000) (log x)))
  (fixed-point f guess))

(define (solution-1.36-damping guess)
  (define (average x y)
    (/ (+ x y) 2))
  (define (f x)
    (average x (/ (log 1000) (log x))))
  (fixed-point f guess))

; output:
;> (solution-1.36 10)
;2.9999999999999996
;6.2877098228681545
;3.7570797902002955
;5.218748919675316
;4.1807977460633134
;4.828902657081293
;4.386936895811029
;4.671722808746095
;4.481109436117821
;4.605567315585735
;4.522955348093164
;4.577201597629606
;4.541325786357399
;4.564940905198754
;4.549347961475409
;4.5596228442307565
;4.552843114094703
;4.55731263660315
;4.554364381825887
;4.556308401465587
;4.555026226620339
;4.55587174038325
;4.555314115211184
;4.555681847896976
;4.555439330395129
;4.555599264136406
;4.555493789937456
;4.555563347820309
;4.555517475527901
;4.555547727376273
;4.555527776815261
;4.555540933824255
;4.555532257016376
;4.555532257016376
;> (solution-1.36-damping 10)
;6.5
;5.095215099176933
;4.668760681281611
;4.57585730576714
;4.559030116711325
;4.55613168520593
;4.555637206157649
;4.55555298754564
;4.555538647701617
;4.555536206185039
;4.555536206185039
;
; We can see that it converges much faster with average damping.

; ========== E1.37

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i)
                         (+ (d i) result)))))
  (iter k 0))

(define (calc-steps-1.37)
  (define (close-enough? result)
    (< (abs (- result (/ (- (sqrt 5) 1) 2))) 0.0001))
  (define (n i) 1.0)
  (define (d i) 1.0)
  (define (finish k result)
    (display k)
    (newline)
    (display result))
  (define (iter k)
    (let ((result (cont-frac-iter n d k)))
      (if (close-enough? result)
          (finish k result)
          (iter (+ 1 k)))))
  (iter 2))

; Output:
; 10
; 0.6179775280898876

(define (cont-frac-rec n d k)
  (define (rec i)
    (cond ((= i k) (/ (n k) (d k)))
          (else (/ (n i) (+ (d i)
                            (rec (+ i 1)))))))
  (rec 1))

; ========== E1.38

(define (solution-1.38 steps)
  (+ (cont-frac-iter (lambda (x) 1.0)
                     (lambda (x)
                       (let ((n (remainder x 3)))
                         (cond ((= n 0) 1)
                               ((= n 1) 1)
                               (else (* 2 (/ (+ 1 x) 3))))))
                     steps)
     2))

; ========== E1.39

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (square x)))
  (define (d i)
    (- (* 2 i) 1))
  (define (iter result i)
    (if (= 0 i)
        result
        (iter (/ (n i) (- (d i) result)) (- i 1))))
  (iter 0 k))

; ========== E1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* (square x) a)
       (* x b)
       c)))

; ========== E1.41

(define inc
  (lambda (x) (+ 1 x)))

(define (double f)
  (lambda (x) (f (f x))))

; ========== E1.42

(define (compose f g)
  (lambda (x) (f (g x))))

; ========== E1.43

(define (repeated-iter f n)
  (lambda (x)
    (if (> n 1)
        ((repeated-iter f (- n 1)) (f x))
        (f x))))

(define (repeated-rec f n)
  (if (= n 1)
      f
      (compose f (repeated-rec f (- n 1)))))

; ========== E1.44

(define (smooth f)
  (define dx 0.00001)
  (define (average a b c)
    (/ (+ a b c) 3))
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (repeated-smooth f n)
  ((repeated-iter smooth n) f))

; ========== E1.45