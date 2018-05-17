#lang racket

; ========== pre-defined procedures

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? obj)
  (eq? (car obj) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE_BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)      ;symbol
                               (cadr pair))    ;frequency
                    (make-leaf-set (cdr pairs))))))

; ========== E2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-1 current)
    (cond ((not (element-of-set? symbol (symbols current)))
           (error "can not encode symbol" symbol))
          ((leaf? current) '())
          ((element-of-set? symbol (symbols (left-branch current)))
           (cons 0 (encode-symbol-1 (left-branch current))))
          ((element-of-set? symbol (symbols (right-branch current)))
           (cons 1 (encode-symbol-1 (right-branch current))))
          (else (error "can not encode symbol" symbol))))
  (encode-symbol-1 tree))

; ========== E2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (cond ((null? leaves) '())
        ((null? (cdr leaves))    ; single element
         (car leaves))
        (else (successive-merge (adjoin-set (make-code-tree (car leaves)
                                                            (cadr leaves))
                                            (cddr leaves))))))

; ========== E2.70

(define alphabet-2.70
  '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define message-2.70
  '(GET A JOB SHA NA NA NA NA NA NA NA NA
        GET A JOB SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))
; (length (encode message-2.70 (generate-huffman-tree alphabet-2.70)))
; => 84
