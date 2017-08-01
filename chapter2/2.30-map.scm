#lang planet neil/sicp

(define (scale-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))