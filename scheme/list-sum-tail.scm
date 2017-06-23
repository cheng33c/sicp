#lang planet neil/sicp

(define (list-sum-tail ls)
  (list-sum-rec ls 0))

(define (list-sum-rec ls sum)
  (if (null? ls)
      sum
      (let ((m (car ls)))
        (list-sum-rec (cdr ls) (+ sum m)))))

(list-sum-tail '(1 2 3 4 5 -1))