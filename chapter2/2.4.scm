#lang planet neil/sicp

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(my-car (my-cons 1 2))

(my-cdr (my-cons 1 2))