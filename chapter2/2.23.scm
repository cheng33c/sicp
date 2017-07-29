#lang planet neil/sicp

;; 这里一定要加上begin 否则会出现错误
;; 原因可能是不用begin 是不是顺序求值
;; 而是使用应用序，先行计算后面(for-each)的值
(define (for-each proc ls)
  (if (null? (cdr ls))
      (proc (car ls))
      (begin
        (proc (car ls))
        (for-each proc (cdr ls)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88 98 63 54))


;;; another method
;;; 这里cond没什么卵用，只是保证这三条表达式按顺序求值
(define (for-each2 p lst)
    (cond ((not (null? lst))
           (p (car lst))
           (for-each p (cdr lst)))))

(for-each2 (lambda (x) (newline) (display x))
           (list 57 321 88 98 63 54))