#lang planet neil/sicp

(define (my-keep-matching-items ls proc)
  (cond
    ((null? ls) '())
    ((proc (car ls))
     (cons (car ls) (my-keep-matching-items (cdr ls) proc)))
    (else
     (my-keep-matching-items (cdr ls) proc))))

(my-keep-matching-items '(1 2 3 4 5 -2 -4 20) positive?)