#lang planet neil/sicp

(define (list-ref item n)
  (if (= n 0)
      (car item)
      (list-ref (cdr item) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)