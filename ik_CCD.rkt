#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(require "util.rkt")
(require "kinematic.rkt")

(define joints 
  (let ((joints (vector))) 
    (begin
      (do ((i 0 (+ i 1)))
        ((= i 6))
        (set! joints (vector-append joints (vector (* 0.8 (- 0.5 (random)))))))
      joints)))

(define data (first-step-inverse-kinematics joints 16 '(120 . 120) '(120 . 30)))

(define (main ws)
  (big-bang ws
            [on-tick (lambda (cw) (set! data (apply step-inverse-kinematics data)))]
            [to-draw (lambda (cw) (draw 
                                   (list-ref data 0) 
                                   (list-ref data 1) 
                                   (list-ref data 2) 
                                   (list-ref data 3) 
                                   (list-ref data 6) 240 240))]
            [on-mouse (lambda (img x y evt)
                        (begin
                          (when (equal? evt "button-down") 
                            (set! data (list-replace-element-at-index data 3 (cons x y))))
                          img))]))

(main 10)

