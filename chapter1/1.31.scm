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


;;; 另一种解 pi / 2 = (* 2 2 4 4 6 6 ...) / (* 3 3 5 5 7 7 ...)
;;; 不能使用。 查错中.

(define (factorial1 n)
  (define (next x) (+ x 2))

  (* (/ (product square 2 next n) (product square 3 next n)) 2))

(newline)