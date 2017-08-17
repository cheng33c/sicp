#lang planet neil/sicp

;; require
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements)))) ;; 其实没差,把前面多构造的一空项去掉

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2))) ;; 计算左子树大小
        (let ((left-result (partial-tree elts left-size))) ;; 递归到最后一项 
          (let ((left-tree (car left-result)) ;; 左子树取左列表的第一项
                (non-left-elts (cdr left-result)) ;; 剩余的左列表赋给`non-left-elts`
                (right-size (- n (+ left-size 1)))) ;; 右子树大小是`(- n (+ left-size 1))`
            (let ((this-entry (car non-left-elts)) ;; 当前项是第一个`non-left-elts`,也是左列表的第二个
                  (right-result (partial-tree (cdr non-left-elts) ;; 右子树是第二个`non-left-elts`
                                              right-size))) ;; 接下来递归调用生成右子树列表
              (let ((right-tree (car right-result)) ;; 右子树取右子树列表第一个
                    (remaining-elts (cdr right-result))) ;; 剩余的项取除右子树列表第一个的
                (cons (make-tree this-entry left-tree right-tree) ;; 构造树, 三个参数是左子,当前,右子
                      remaining-elts)))))))) ;; 与剩余列表项连接并返回

;; main
(define (union-set a b) 
  (cond ((null? a) b) 
        ((null? b) a) 
        (else 
         (let ((a-entry (entry a)) 
               (a-left-branch (left-branch a)) 
               (a-right-branch (right-branch a)) 
               (b-entry (entry b)) 
               (b-left-branch (left-branch b)) 
               (b-right-branch (right-branch b))) 
           (cond ((= a-entry b-entry) 
                  (make-tree a-entry 
                             (union-set a-left-branch b-left-branch) 
                             (union-set a-right-branch b-right-branch))) 
                 ((< a-entry b-entry) 
                  (make-tree b-entry 
                             (union-set a b-left-branch) 
                             b-right-branch)) 
                 ((> a-entry b-entry) 
                  (make-tree a-entry 
                             (union-set a-left-branch b) 
                             a-right-branch))))))) 
  
(union-set (list->tree '(1 3 5)) 
           (list->tree '(2 3 4))) 
;; => (3 (2 (1 () ()) ()) (5 (4 () ()) ())) 
  
;warning this algo has wrong output 
;; check input as bst1 bst2 as: 
; (define bst1 (list->tree '(1 2 3 4 5 6 7))) 
; (define bst2 (list->tree '(6 7 8 9))) 
(define (intersection-set a b) 
  (cond ((null? a) '())
        ((null? b) '())
        (else 
         (let ((a-entry (entry a)) 
               (a-left-branch (left-branch a)) 
               (a-right-branch (right-branch a)) 
               (b-entry (entry b)) 
               (b-left-branch (left-branch b)) 
               (b-right-branch (right-branch b))) 
           (cond ((= a-entry b-entry) 
                  (make-tree a-entry 
                             (intersection-set a-left-branch b-left-branch) 
                             (intersection-set a-right-branch b-right-branch))) 
                 ((< a-entry b-entry) 
                  (union-set 
                   (intersection-set a-right-branch 
                                     (make-tree b-entry '() b-right-branch)) 
                   (intersection-set (make-tree a-entry a-left-branch '()) 
                                     b-left-branch))) 
                 ((> a-entry b-entry) 
                  (union-set 
                   (intersection-set (make-tree a-entry '() a-right-branch) 
                                     b-right-branch) 
                   (intersection-set a-left-branch 
                                     (make-tree b-entry b-left-branch '()))))))))) 
  
(intersection-set (list->tree '(3 5 10)) 
                  (list->tree '(1 2 3 4 5 7))) 
;; => (5 (3 () ()) ()) 