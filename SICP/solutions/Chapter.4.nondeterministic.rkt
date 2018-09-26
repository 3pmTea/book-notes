; ========== pre-defined procedures
(define (require p) (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (cadr x) (car (cdr x)))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))))

(define (accumulate f init-value l)
  (if (null? l)
      init-value
      (f (car l) (accumulate f init-value (cdr l)))))

(define (first-of predicate l)
  (cond ((null? l) false)
        ((predicate (car l)) (car l))
        (else (first-of predicate (cdr l)))))

;; The logic puzzle example
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

; ========== E4.35
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

; for test:
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; ========== E4.36
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (all-pythagorean-triples)
  (let ((sum (an-integer-starting-from 3)))
    (let ((i (an-integer-between 1 (/ sum 3))))
      (let ((j (an-integer-between i (/ sum 2))))
        (let ((k (- sum i j)))
          (require (<= j k))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k))))))

; ========== E4.40
(define (multiple-dwellin-4.40)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require
            (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

; ========== E4.42
(define (liars)
  (define (xor p q)
    (if p (not q) q))
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

; ========== E4.43
(define (yacht-puzzle)
  (define (daughter-of father) (car father))
  (define (yacht-of father) (car (cdr father)))
  (let ((Moore (list
                (amb 'Mary) ; replace this line if we are not told about Mary's family name
                ; (amb 'Mary 'Lorna 'Melissa 'Rosalind 'Gabrielle)
                (amb 'Lorna))))
    (let ((Downing (list (amb 'Mary 'Lorna 'Melissa 'Rosalind 'Gabrielle)
                         (amb 'Melissa))))
      (let ((Hall (list (amb 'Mary 'Lorna 'Melissa 'Rosalind 'Gabrielle)
                        (amb 'Rosalind))))
        (let ((Barnacle (list (amb 'Mary 'Lorna 'Melissa 'Rosalind 'Gabrielle)
                              (amb 'Gabrielle))))
          (require (eq? (daughter-of Barnacle) (yacht-of Downing)))
          (let ((Parker (list (amb 'Mary 'Lorna 'Melissa 'Rosalind 'Gabrielle)
                              (amb 'Mary 'Lorna 'Melissa 'Rosalind 'Gabrielle))))
            (require (distinct? (map daughter-of
                                     (list Moore Downing Hall Barnacle Parker))))
            (require (distinct? (map yacht-of
                                     (list Moore Downing Hall Barnacle Parker))))
            (require (accumulate (lambda (x y)
                                   (and y (not (eq? (daughter-of x) (yacht-of x)))))
                                 true
                                 (list Moore Downing Hall Barnacle Parker)))
            (let ((father-of-Gabrielle
                   (first-of (lambda (x)
                               (eq? (daughter-of x) 'Gabrielle))
                             (list Moore Downing Hall Barnacle))))
              (require (and father-of-Gabrielle
                            (eq? (daughter-of Parker)
                                 (yacht-of father-of-Gabrielle))))
              (list (cons 'Moore Moore)
                    (cons 'Downing Downing)
                    (cons 'Hall Hall)
                    (cons 'Barnacle Barnacle)
                    (cons 'Parker Parker)))))))))

; ========== E4.44
; simple but slow
(define (eight-queens)
  (define (try-one row)
    (list row (amb 1 2 3 4 5 6 7 8)))
  (define (iter i)
    (cons (try-one i)
          (if (>= i 8)
              '()
              (iter (+ i 1)))))
  (let ((trial (iter 1)))
    (require (distinct? (map cadr trial)))
    (require (distinct? (map (lambda (x)
                               (+ (car x) (cadr x)))
                             trial)))
    (require (distinct? (map (lambda (x)
                               (- (car x) (cadr x)))
                             trial)))
    trial))

; a faster version
(define (eight-queens-2)
  (define (iter predicate l)
    (cond ((null? l) false)
          ((predicate (car l)) true)
          (else (iter predicate (cdr l)))))
  (define (safe? this rest)
    (not (or (iter (lambda (x)
                     (= (cadr this)
                        (cadr x)))
                   rest)
             (iter (lambda (x)
                     (= (+ (car this) (cadr this))
                        (+ (car x) (cadr x))))
                   rest)
             (iter (lambda (x)
                     (= (- (car this) (cadr this))
                        (- (car x) (cadr x))))
                   rest))))
  (define (queen n)
    (define (try i rest)
      (if (<= i 0)
          rest
          (let ((this (list i (an-integer-between 1 n))))
            (require (safe? this rest))
            (try (- i 1) (cons this rest)))))
    (try n '()))
  (queen 8))

; ========== NLP utilities
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent))

