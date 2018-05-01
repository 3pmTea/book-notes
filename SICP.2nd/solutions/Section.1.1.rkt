; ==========

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (solution-1.3 a b c)
  (cond ((and (<= a b) (<= a c)) (sum-of-squares b c))
        ((and (<= b a) (<= b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))

; ==========

; Solution-1.7
; Since 'new-if' is not a special form, with applicative-order evaluation performed,
; the recursive 'improve' clause will be evaluated whether or not the guess is good enough.
; Thus leading to stack overflow.

; ==========

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

(define (solution-1.8 x) (cube-root x))

; ==========

