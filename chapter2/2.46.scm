#lang planet neil/sicp

(define (make-vect x y) (list x y))

(define (xcor-vect vec) (car vec))

(define (ycor-vect vec) (cadr vec))

(define (add-vect vec1 vec2)
  (cons (+ (xcor-vect vec1) (xcor-vect vec2))
        (+ (ycor-vect vec1) (ycor-vect vec2))))

(define (sub-vect vec1 vec2)
  (cons (- (xcor-vect vec1) (xcor-vect vec2))
        (- (ycor-vect vec1) (ycor-vect vec2))))

(define (scale-vect s vec)
  (cons (* (xcor-vect vec) s)
        (* (ycor-vect vec) s)))



;; test
(define vec1 (make-vect 2 9))
(define vec2 (make-vect 1 3))
(define s 7)

(xcor-vect vec1)
(ycor-vect vec2)
(add-vect vec1 vec2)
(sub-vect vec1 vec2)
(scale-vect s vec1)