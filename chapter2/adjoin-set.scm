#lang planet neil/sicp

;; require
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; main part
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; test part
(adjoin-set 10 '(1 2 10))
(adjoin-set 20 '(2 3))