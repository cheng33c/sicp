#lang planet neil/sicp

(define (ls-sum-letrec ls)
  (letrec ((iter (lambda (ls sum)
                   (if (null? ls)
                       sum
                       (iter (cdr ls) (+ sum (car ls)))))))
    (iter ls 0)))

(ls-sum-letrec '(1 2 3 4 4 5))