#lang planet neil/sicp

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (improve y x)
    (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 1))

(define (my-cube-root y x)
    (if (good-enough? y x)
        y
        (my-cube-root (improve y x)
                x)))


(define (cube-root x)
    (my-cube-root 1.0 x))

(cube-root 64)