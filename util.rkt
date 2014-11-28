#lang racket

(provide list-replace-element-at-index)
(provide vector-last)
(provide vector-sum)
(provide distance)
(provide normalize-angle)
(provide clamp)

(define (list-replace-element-at-index list index val)
  "replaces an element of a list, does not mutate input list, makes a new list"
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
  "last item of a vector"
  (vector-ref vec (- (vector-length vec) 1)))

(define (vector-sum vec n) 
  "sum of the n first elements of a vector"
  (let ((acc 0))
    (begin
      (do ((i 0 (+ i 1)))
        ((= i n))
        (set! acc (+ acc (vector-ref vec i))))
      acc)))

(define (distance a b) 
  "distance between to points (each point being represented by a (x . y) pair)"
  (let* ((dx (- (car a) (car b)))
         (dy (- (cdr a) (cdr b)))) 
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (normalize-angle angle)
  "angle (radians) in a [-pi, pi] range"
  (cond 
    ((> angle pi) (normalize-angle (- angle (* 2.0 pi))))
    ((< angle (* -1.0 pi)) (normalize-angle (+ angle (* 2.0 pi))))
    (else angle)))

(define (clamp val low up)
  "clamped value of a number"
  (max low (min up val)))