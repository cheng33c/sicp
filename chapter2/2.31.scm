#lang planet neil/sicp

(define (tree-map produce tree)
  (map (lambda (tree)
         (if (pair? tree)
             (tree-map produce tree)
             (produce tree)))
       tree))

(define (square x) (* x x))

(define mytree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree tree) (tree-map square tree))

(square-tree mytree)