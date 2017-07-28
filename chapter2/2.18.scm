#lang planet neil/sicp

(define (reverse my)
  (if (null? my)
      my
      (cons (reverse (cdr my)) (car my))))

(reverse (list 1 4 9 16 25))