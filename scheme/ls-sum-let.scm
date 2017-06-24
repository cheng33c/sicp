#lang planet neil/sicp

(define (ls-sum-let ls)
  (let loop((ls0 ls) (sum 0))
    (if (null? ls0)
        sum
        (loop (cdr ls0) (+ sum (car ls0))))))

(ls-sum-let '(1 2 3 4))