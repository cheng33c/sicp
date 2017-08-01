#lang planet neil/sicp

;; 构造偶数斐波纳契数
;; 这个程序并不是可以运行的
;; fib没有定义
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(even-fibs 30)