;; 赶脚没有我的3.18解法快
;; 参考sicp.readthedocs.io的解决方法

(load "3.13.scm")

(define (loop? ls)
  (define (iter ls-status1 ls-status2)
    (let ((status1 (car ls-status1))
          (status2 (car ls-status2)))
      (cond ((null? status1) #f)
            ((eq? status1 status2) #t)
            (else
             (iter (cdr ls-status1)
                   (cddr ls-status2))))))
  (iter ls (cdr ls)))

(define z (make-cycle (list 'a 'b 'c 'd)))
