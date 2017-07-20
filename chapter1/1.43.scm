#lang planet neil/sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f times)
  (define (iter repeated-f time)
    (if (= time 1)
        repeated-f
        (iter (compose f repeated-f) (- time 1))))
  (iter f times))

(define (square x) (* x x))
((repeated square 2) 5)