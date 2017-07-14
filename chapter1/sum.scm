#lang planet neil/sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


;;; 两数之间所有的数的立方和相加
(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))

;;; 两数之间所有的数相加
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

;;; 莱布尼茨收敛
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(sum-cubes 1 10)
(sum-integers 1 10)
(* 8 (pi-sum 1 1000))