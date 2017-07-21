#lang planet neil/sicp

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (let ((y (improve guess)))
      (if (good-enough? guess y)
          y
          ((iterative-improve good-enough? improve) y)))))

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))


(define (good-enough? a b)
  (define tolerance 0.000001) 
  (< (abs (- a b)) tolerance))

(define (sqrt x)
  ((iterative-improve
    good-enough?
    (lambda (y)
      (/ (+ (/ x y) y) 2))) 1.0))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (improve guess)
        (f guess))
    ((iterative-improve close-enough? improve) first-guess))



(sqrt 9)
(fixed-point cos 1.0)
