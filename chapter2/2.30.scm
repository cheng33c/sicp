#lang planet neil/sicp

(define (scale-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (scale-tree (car tree))
                    (scale-tree (cdr tree))))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))