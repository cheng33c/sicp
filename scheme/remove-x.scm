#lang planet neil/sicp

(define (remove-x ls x)
  (if (null? ls)
      '()
      (let ((h (car ls)))
        ((if (eqv? x h)
             (lambda (y) y)
             (lambda (y) (cons h y)))
         (remove-x (cdr ls) x)))))

(remove-x '(1 2 3 4 1 1 3 4 1 5) 1)