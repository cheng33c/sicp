#lang planet neil/sicp



;; 只能对二叉树进行反转
(define (deep-reverse tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) tree)
        (else
         (reverse (list (deep-reverse (car tree))
                        (deep-reverse (car (cdr tree))))))))

(define x (list (list 1 (list 3 4)) (list 5 (list 7 (list 10 9)))))

(deep-reverse x)





;; 多叉树反转
(define (tree-reverse lst)
  (define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (if (pair? (car remained-items))
                        (tree-reverse (car remained-items))
                        (car remained-items))
                    result))))
  (iter lst '()))

;; 处理多叉树
(define y (list (list 1 2 (list 3 4)) (list 5 6 20 (list 7 8 (list 10 9)))))
(tree-reverse y)

(tree-reverse (list (list 1 2) (list 3 4) (list 5 6)))





;; 二叉树反转另一种写法
(define (deep-reverse1 tree)
  (cond ((empty-tree? tree)
         '())
        ((leaf? tree)
         tree)
        (else
         (reverse (make-tree (deep-reverse1 (left-branch tree))
                             (deep-reverse1 (right-branch tree)))))))

(define (empty-tree? tree)
  (null? tree))

(define (leaf? tree)
  (not (pair? tree)))

(define (make-tree left-branch right-branch)
  (list left-branch right-branch))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))