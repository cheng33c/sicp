#lang planet neil/sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (or (< n 0) (< d 0))
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(print-rat (make-rat 2 -4))