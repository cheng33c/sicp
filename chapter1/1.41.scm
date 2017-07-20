#lang planet neil/sicp

(define (double f)
    (lambda (x)
        (f (f x))))

(define (inc x) (+ x 1))
((double inc) 1)
(((double (double double)) inc) 5)

;;; 进行三次调用，调用了2^3，所以加上了（2^3）*2=16
;;; 最终结果就是16+5=21

;;; 推导
;;; ((double (double (double (double inc)))) 5) 
;;; ((double (double (double (lambda (x) (inc (inc x)))))) 5)
;;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))))) 