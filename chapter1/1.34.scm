#lang planet neil/sicp

(define (f g)
  (g 2))

(f f)

;;; 返回错误
;;; 原因是 你展开(f f)的调用过程
;;; 到最后2会以函数的方式调用，所以返回错误