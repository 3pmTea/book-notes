#lang racket

; dispatching utils
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (type-tag x) (car x))
(define (contents x) (cdr x))

; predicates
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

; ========== E2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; b) derivatives of sums and products
(define (install-sum-deriv-package)
  (define (make-sum a b)
    (cond ((=number? a 0) b)
          ((=number? b 0) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b))))
  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'deriv '+ deriv-sum)
  'done)

(define (install-prod-deriv-package)
  (define (make-product a b)
    (cond ((or (=number? a 0) (=number? b 0)) 0)
          ((=number? a 1) b)
          ((=number? b 1) a)
          ((and (number? a) (number? b)) (* a b))
          (else (list '* a b))))
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cadr exp))
  (define (deriv-prod exp var)
    (get '
