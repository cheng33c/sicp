#lang planet neil/sicp

;; require
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((or (number? set1) (number? set2))
         (error "number is not set"))
        (else
         (let ((current1 (car set1))
               (current2 (car set2))
               (remain1 (cdr set1))
               (remain2 (cdr set2)))
           (cond ((< current1 current2)
                  (cons current1
                        (union-set remain1 set2)))
                 ((= current1 current2)
                  (cons current1
                        (union-set remain1 remain2)))
                 ((> current1 current2)
                  (cons current2
                        (union-set set1 remain2))))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

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

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (intersection-tree tree another)
  (list->tree
   (intersection-set (tree->list-2 tree)
                     (tree->list-2 another))))

(define (union-tree tree another)
  (list->tree
   (union-set (tree->list-2 tree)
              (tree->list-2 another))))

;; test
(define it (intersection-tree (list->tree '(1 2 3 4 5))
                              (list->tree '(1 3 5 7 9))))
(define ut (union-tree (list->tree '(1 2 3 4 5))
                       (list->tree '(1 3 5 7 9))))

(display it)
(newline)
(display ut)