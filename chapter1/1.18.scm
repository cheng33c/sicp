#lang planet neil/sicp
(define (double x) (* 2 x))
(define (halve x) (if (even? x) (/ x 2)))

(define (fast-expt-add b n)
  (define (fast-expt b n result)
    (cond ((= n 0) result)
          ((even? n) (fast-expt (double b) (halve n) result))
          (else (fast-expt b (- n 1) (+ b result)))))
  (fast-expt b n 0))

(fast-expt-add 2 4)