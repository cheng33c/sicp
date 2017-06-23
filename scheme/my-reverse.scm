#lang planet neil/sicp

(define (my-reverse ls)
  (my-reverse-rec ls '()))

(define (my-reverse-rec ls0 ls1)
  (if (null? ls0)
      ls1
      (my-reverse-rec (cdr ls0) (cons (car ls0) ls1))))

(my-reverse '(1 2 3 4 5 6 7 8 9))