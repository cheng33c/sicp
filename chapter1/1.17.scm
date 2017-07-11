#lang planet neil/sicp
(define (double x) (* 2 x))
(define (halve x) (if (even? x) (/ x 2)))

(define (fast-expt b n)
  (cond ((= b 0) 0)
        ((even? n) (double (fast-expt b (halve n))))
        (else (+ b (* b (- n 1))))))

(fast-expt 2 5)