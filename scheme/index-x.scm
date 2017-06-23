#lang planet neil/sicp
(define (index-x ls x index)
  (cond
    ((null? ls) #f)
    ((eqv? (car ls) x) index)
    (else (index-x (cdr ls) x (+ index 1)))))

(define (index-x-if ls x index)
  (if (eqv? (car ls) x) index)
  (if (null? ls) #f)
  (index-x (cdr ls) x (+ index 1)))

(index-x-if '(1 2 3 4 5 6) 1 0)
(index-x '(1 2 3 4 5 6) 1 0)