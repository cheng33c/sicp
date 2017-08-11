#lang planet neil/sicp

(define (my-equal? arg1 arg2)
  (if (and (pair? arg1)
           (pair? arg2))
      (and (eq? (car arg1) (car arg2))
           (equal? (cdr arg1) (cdr arg2)))
      (eq? arg1 arg2)))

(my-equal? 1 2)
(my-equal? 1 1)
(my-equal? (list 1 2 3) (list 1 2 3))
(my-equal? (list 1 2 3) (list 1 2 3 4))
(my-equal? '(this is a list) '(this is a list))
(my-equal? '(this is a list) '(this (is a) list))