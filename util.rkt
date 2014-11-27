#lang racket

(provide list-replace-element-at-index)
(provide vector-last)
(provide vector-sum)
(provide distance)
(provide normalize-angle)
(provide range)

(define (list-replace-element-at-index list index val)
  (let loop ((lst list)
             (rv '())
             (i 0))
    (if (null? lst)
      (reverse rv)
      (loop 
        (cdr lst) 
        (cons (if (= index i) val (car lst)) rv)
        (+ i 1)))))

(define (vector-last vec) 
  (vector-ref vec (- (vector-length vec) 1)))

(define (vector-sum vec count) 
  (let ((acc 0)) 
    (begin
      (do ((i 0 (+ i 1)))
        ((= i count))
        (set! acc (+ acc (vector-ref vec i))))
      acc)))

(define (distance a b) 
  (let* ((dx (- (car a) (car b)))
         (dy (- (cdr a) (cdr b)))) 
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (normalize-angle angle)
  (cond 
    ((> angle pi) (normalize-angle (- angle (* 2.0 pi))))
    ((< angle (* -1.0 pi)) (normalize-angle (+ angle (* 2.0 pi))))
    (else angle)))

(define (range val low up)
  (max low (min up val)))