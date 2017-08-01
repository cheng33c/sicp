#lang planet neil/sicp

;; 计算奇数叶子平方和
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (square x) (* x x))
(define mytree (list 1 (list 2 3 4) (list 5 6 7) 8 (list 20 30)))

(sum-odd-squares mytree)