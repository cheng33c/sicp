#lang planet neil/sicp

(define (my-sum-do ls)
  (do ((ls0 ls (cdr ls0)) (n 0 (+ n (car ls0))))
    ((null? ls0) n)))

(my-sum-do '(1 2 3))