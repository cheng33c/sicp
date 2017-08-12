#lang planet neil/sicp

;; require
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; main

;; recurvise
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;; iteration
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; test
(define tree1 (make-tree 1 (make-tree 3 '() '()) (make-tree 4 '() '())))
(define tree2 (make-tree 20 (make-tree 10 '() '()) (make-tree 40 '() '())))
(define tree3 (make-tree 13 tree1 tree2))
(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)


;; 图2-16
(define tree4 (make-tree 7
                         (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                         (make-tree 9 nil (make-tree 11 '() '()))))
(define tree5 (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9 '() (make-tree 11 '() '())))))
(define tree6 (make-tree 5
                         (make-tree 3 (make-tree 1 '() '()) '())
                         (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))
(tree->list-1 tree4)
(tree->list-2 tree4)
(tree->list-1 tree5)
(tree->list-2 tree5)
(tree->list-1 tree6)
(tree->list-2 tree6)



;; 产生相同结果
;; 不是。迭代的过程增长的慢