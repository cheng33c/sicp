#lang planet neil/sicp

(define (make-vect x y) (list x y))

(define (xcor-vect vec) (car vec))

(define (ycor-vect vec) (cadr vec))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))


(define (make-segment vec1 vec2)
  (list vec1 vec2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))


;; test
(define vec1 (make-vect 2 9))
(define vec2 (make-vect 1 3))

(define seg (make-segment vec1 vec2))
(start-segment seg)
(end-segment seg)