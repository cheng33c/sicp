#lang planet neil/sicp

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-vect1 vec) (car vec))

(define (edge1-vect1 vec) (cadr vec))

(define (edge2-vect1 vec) (caddr vec))




(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-vect2 vec) (car vec))

(define (edge1-vect2 vec) (cadr vec))

(define (edge2-vect2 vec) (caddr vec))

;; test make-frame1 select functions
(define vec-frame1 (make-frame1 3 5 7))
(origin-vect1 vec-frame1)
(edge1-vect1 vec-frame1)
(edge2-vect1 vec-frame1)

;; test make-frame2 select functions
(define vec-frame2 (make-frame2 3 5 7))
(origin-vect2 vec-frame1)
(edge1-vect2 vec-frame1)
(edge2-vect2 vec-frame1)