#lang planet neil/sicp


(define (square x) (* x x))

(define (product term a next b)
  (define (mult a result)
    (if (> a b)
        result
        (mult (next a) (* result (term a)))))
  (mult a 1))

(define (factorial b)
  (define (inc2 x) (+ x 2))
  
  (* (/ (* (product square 4 inc2 b) 2) (product square 3 inc2 b)) 4))

(factorial 30)