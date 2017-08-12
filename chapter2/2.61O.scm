#lang planet neil/sicp

;; require 2.62 `union-set`

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((or (number? set1) (number? set2))
         (error "number is not set"))
        (else
         (let ((current1 (car set1))
               (current2 (car set2))
               (remain1 (cdr set1))
               (remain2 (cdr set2)))
           (cond ((< current1 current2)
                  (cons current1
                         (union-set remain1 set2)))
                 ((= current1 current2)
                  (cons current1
                        (union-set remain1 remain2)))
                 ((> current1 current2)
                  (cons current2
                        (union-set set1 remain2))))))))

;; main
(define (adjoin-set x set)
  (union-set (list x) set))

(adjoin-set 5 '(1 2 4 5 3))
(adjoin-set 7 '(2))