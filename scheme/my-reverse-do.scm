#lang planet neil/sicp

(define (my-reverse-do ls)
  (do ((ls0 ls (cdr ls0)) (ls1 '() (cons (car ls0) ls1)))
    ((null? ls0) ls1)))

(define (my-reverse-do1 ls)
  (do ((ls0 ls (cdr ls0)) (ls1 '() (cons (car ls0) ls1)))
    ((null? ls0) ls1)))

(my-reverse-do '(2 3 4))