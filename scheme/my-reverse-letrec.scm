#lang planet neil/sicp

(define (my-reverse-letrec ls)
  (letrec ((iter (lambda (ls0 ls1)
                   (if (null? ls1)
                       ls0
                       (iter (cons (car ls1) ls0) (cdr ls1))))))
    (iter '() ls)))

(define (my-reverse-letrec2 ls)
  (letrec ((iter (lambda (ls0 ls1)
                   (if (null? ls0)
                       ls
                       (iter (cdr ls0) (cons (car ls0) ls1))))))
    (iter ls '())))

(my-reverse-letrec '(1 2 3 4))