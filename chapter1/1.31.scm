#lang planet neil/sicp

(define (square x) (* x x))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial b)
  (define (inc2 x) (+ x 2))

  (define (number-term i)
    (cond ((= i 1) 2)
          ((even? i) (+ i 2))
          (else (+ i 1))))

  (define (denom-term i)
    (if (odd? i)
        (+ i 2)
        (+ i 1)))
  
  (* (/ (product number-term 1 (lambda (i) (+ i 1)) b)
        (product denom-term 1 (lambda (i) (+ i 1)) b))
     4))

(exact->inexact (factorial 200))