#lang planet neil/sicp

;; implement by O(n^2)

;; require part
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; main part
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) set2)
        ((or (number? set1) (number? set2))
         (error "number is not set"))
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (cons (car set1) set2)))))

;; test part
(union-set '(1 2 3) '(4 5 6))
(union-set '(1 2 3) '(4 9 1))
(union-set 1 '(2 3))