#lang planet neil/sicp

(define (my-remove-x ls x)
  (if (null? ls)
      '()
      (let ((ls0 (car ls)))
        ((if (eqv? ls0 x)
             (lambda (y) y)
             (lambda (y) (cons ls0 y)))
         (my-remove-x (cdr ls) x)))))

(my-remove-x '(1 2 3) 3)