#lang racket
(require sicp-pict)

; ========== pre-defined functions
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; ========== E2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; ========== E2.45
(define (split main part)
  (define (split-rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-rec painter (- n 1))))
              (main painter (part smaller smaller)))))
  split-rec)

(define right-split-2.45 (split beside below))
(define up-split-2.45 (split below beside))

; ========== E2.46
(define (make-vect-2.46 x y)
  (list x y))
(define (add-vect-2.46 v1 v2)
  (map + v1 v2))
(define (sub-vect-2.46 v1 v2)
  (map - v1 v2))
(define (scale-vect-2.46 n v)
  (map (lambda (x) (* n x)) v))

; ========== E2.47
(define (make-frame-l origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-frame-l car)
(define edge1-frame-l cadr)
(define edge2-frame-l caddr)

(define (make-frame-c origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame-c car)
(define edge1-frame-c cadr)
(define edge2-frame-c cddr)

; ========== E2.48
(define (make-segment-2.48 start end)
  (list start end))
(define (start-segment-2.48 seg)
  (car seg))
(define (end-segment-2.48 seg)
  (cadr seg))

; ========== E2.49
(define bl-2.49 (make-vect 0 0))
(define br-2.49 (make-vect 1 0))
(define tl-2.49 (make-vect 0 1))
(define tr-2.49 (make-vect 1 1))
(define tmid-2.49 (make-vect 0.5 1))
(define lmid-2.49 (make-vect 0 0.5))
(define rmid-2.49 (make-vect 1 0.5))
(define bmid-2.49 (make-vect 0.5 0))

(define (connect-vect vect-list)
  (segments->painter (map make-segment vect-list
                         (append (cdr vect-list) (list (car vect-list))))))

(define outliner-2.49
  (connect-vect (list bl-2.49 br-2.49 tr-2.49 tl-2.49)))

(define draw-X-2.49
  (segments->painter (list (make-segment bl-2.49 tr-2.49)
                           (make-segment br-2.49 tl-2.49))))
(define draw-diamond-2.49
  (connect-vect (list tmid-2.49 rmid-2.49 bmid-2.49 lmid-2.49)))

; ========== E2.50
(define flip-horiz-2.50
  (transform-painter (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
(define rotate180-2.50
  (transform-painter (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define rotate270-2.50
  (transform-painter (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

; ========== E2.51
(define (below-2.51-1 bottom-painter top-painter)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom
           ((transform-painter (make-vect 0 0)
                               (make-vect 1 0)
                               split-point)
            bottom-painter))
          (paint-top
           ((transform-painter split-point
                               (make-vect 1 0.5)
                               (make-vect 0 1))
            top-painter)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below-2.51-2 bottom-painter top-painter)
  (rotate270 (beside (rotate90 bottom-painter)
                     (rotate90 top-painter))))
