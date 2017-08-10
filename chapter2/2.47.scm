#lang planet neil/sicp

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 frame) (car frame))

(define (edge1-frame1 frame) (cadr frame))

(define (edge2-frame1 frame) (caddr frame))




(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame) (car frame))

(define (edge1-frame2 frame) (cadr frame))

(define (edge2-frame2 frame) (caddr frame))

;; test make-frame1 select functions
(define vec-frame1 (make-frame1 3 5 7))
(origin-frame1 vec-frame1)
(edge1-frame1 vec-frame1)
(edge2-frame1 vec-frame1)

;; test make-frame2 select functions
(define vec-frame2 (make-frame2 3 5 7))
(origin-frame2 vec-frame1)
(edge1-frame2 vec-frame1)
(edge2-frame2 vec-frame1)