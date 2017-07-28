#lang planet neil/sicp

(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

(my-append (list 1 2 9 100) (list 9 50 39.75 26))