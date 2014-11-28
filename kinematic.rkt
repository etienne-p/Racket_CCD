#lang racket

(require 2htdp/image)

(require "util.rkt")

(provide first-step-inverse-kinematics)
(provide step-inverse-kinematics)
(provide draw)
    
(define (joints-to-cartesian-points vec-joints r origin)
  "cartesian coordinates of arms's origin, pivots and end point"
                      ; should it be rewritten iterating on the vector?
  (let loop ((joints (vector->list vec-joints))
             (points (vector origin))
             (acc-angle 0)
             (offset origin))
    (if (null? joints)
        points
        (let* ((angle (+ acc-angle (car joints)))
               (point (cons
                       (+ (car offset) (* r (cos angle)))
                       (+ (cdr offset) (* r (sin angle))))))
          (loop
           (cdr joints)          ; BiwaScheme: (vector-append points point)
           (vector-append points (vector point))
           angle
           point)))))

(define (end-point-position joints r origin)
  "position of the arm end point"
  (vector-last (joints-to-cartesian-points joints r origin)))

(define (angle-to-target joints r origin joint-index target)
  "angle between (current pivot)->(target) and (current pivot)->(end point) vectors"
  (let* ((cartesian (joints-to-cartesian-points joints r origin))
         (joint-coords (vector-ref cartesian joint-index))
         (end-coords (vector-ref cartesian (vector-length joints)))
         (joint-orientation (atan
                              (- (cdr end-coords) (cdr joint-coords)) 
                              (- (car end-coords) (car joint-coords))))
         (target-orientation (atan
                              (- (cdr target) (cdr joint-coords)) 
                              (- (car target) (car joint-coords))))) 
    (- target-orientation joint-orientation)))

(define (eval-joint-delta joints r origin joint-index target) 
  "evaluates the angle to move the current joint"
  (clamp (normalize-angle (angle-to-target joints r origin joint-index target)) -0.1 0.1))

(define (first-step-inverse-kinematics joints r origin target)
   "compute first step of inverse kinematics, return data to compute the following step"
  (let* ((last-index (- (vector-length joints) 1))
         (joint-delta (eval-joint-delta joints r origin last-index target)))
    (begin 
      (display joint-delta)
      (step-inverse-kinematics joints r origin target +inf.0 joint-delta last-index))))

(define (step-inverse-kinematics joints r origin target error joint-delta joint-index)
  "compute one step of inverse kinematics, return data to compute the following step"
  (let ((new-joint-value (+ (vector-ref joints joint-index) joint-delta)))
    (begin
      (vector-set! joints joint-index new-joint-value)
      (let* ((new-error (distance target (end-point-position joints r origin)))
             (better (< new-error error))
             (joint-change (not better))
             (new-joint-index (if better
                                  joint-index
                                  (if (< joint-index 1)
                                      (- (vector-length joints) 1)
                                      (- joint-index 1)) )))
        (list joints r origin target
              new-error
              (if better joint-delta (eval-joint-delta joints r origin new-joint-index target))
              new-joint-index)))))

(define (draw joints r origin target joint-index scene-width scene-height)
  "draw the current skeleton & target on an image"
  (let ((cartesian (joints-to-cartesian-points joints r origin))
        (count (vector-length joints))
        (scene (empty-scene scene-width scene-height))) 
    (begin
      (do ((i 0 (+ i 1)))
        ((= i count))
        (let ((point (vector-ref cartesian i))
              (next-point (vector-ref cartesian (+ i 1)))
              (color (if (= i joint-index) "red" "blue")))
          (begin
            (set! scene (place-image (circle 4 "solid" color) (car point) (cdr point) scene))
            (set! scene (add-line scene (car point) (cdr point) (car next-point) (cdr next-point) color)))))    
      (set! scene (place-image (circle 4 "solid" "red") (car target) (cdr target) scene))
      scene)))
