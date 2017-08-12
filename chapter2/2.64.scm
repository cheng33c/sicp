#lang planet neil/sicp

;; require
(define (make-tree entry left right)
  (list entry left right))

;; main
;; problem a)
(define (list->tree elements)
  (car (partial-tree elements (length elements)))) ;; 其实没差，把前面多构造的一空项去掉

(define (list->tree2 elements)
  (partial-tree elements (length elements)))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2))) ;; 计算左子树大小
        (let ((left-result (partial-tree elts left-size))) ;; 递归到最后一项 
          (let ((left-tree (car left-result)) ;; 左子树取左列表的第一项
                (non-left-elts (cdr left-result)) ;; 剩余的左列表赋给`non-left-elts`
                (right-size (- n (+ left-size 1)))) ;; 右子树大小是`(- n (+ left-size 1))`
            (let ((this-entry (car non-left-elts)) ;; 当前项是第一个`non-left-elts`，也是左列表的第二个
                  (right-result (partial-tree (cdr non-left-elts) ;; 右子树是第二个`non-left-elts`
                                              right-size))) ;; 接下来递归调用生成右子树列表
              (let ((right-tree (car right-result)) ;; 右子树取右子树列表第一个
                    (remaining-elts (cdr right-result))) ;; 剩余的项取除右子树列表第一个的
                (cons (make-tree this-entry left-tree right-tree) ;; 构造树， 三个参数是左子，当前，右子
                      remaining-elts)))))))) ;; 与剩余列表项连接并返回

;; problem b)  result:  O(n)


;; test
(list->tree (list 1 3 5 7 9 11))
(list->tree2 (list 1 3 5 7 9 11))