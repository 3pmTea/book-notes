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

; ==========