#lang planet neil/sicp

;; require part
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; main part
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; test part
(intersection-set '(1 2 3) '(1 2 3))
(intersection-set '(2 3) '(9 8 7))
(intersection-set '(1 9 20) '(20 35 40))
(intersection-set nil nil)