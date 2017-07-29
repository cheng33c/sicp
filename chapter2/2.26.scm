#lang planet neil/sicp

(define x (list 1 2 3))

(define y (list 4 5 6 7))

(define a (append x y))
(define c (cons x y))
(define l (list x y))

a
c
l

(length a)
(length c)
(length l)