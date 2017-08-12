#lang planet neil/sicp

;; 这里set1 set2都是非重复的集合
;; 如果有重复的话在`(= current1 current2)`这里可能有错误

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

(union-set '(1 2 3) '(4 5 6))
(union-set '(1 2 4) '(4))
(union-set '(2 3 9) '(6 7))