#lang planet neil/sicp

;; `删除某个元素`操作较少，但`添加查找`操作较多且元素较多
;; 速度不好说。如果某元素在表中数量多，查找速度会快
;; 但如果某个元素数量很少，那么在有重复元素的表中查找可能会更慢
;; 实际操作中更倾向于**只有一种元素**的表示

;; not change part
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

;; main part
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) set2)
        ((or (number? set1) (number? set2))
         (error "number is not set"))
        (else (union-set (cdr set1)
                         (cons (car set1) set2)))))

(define (adjoin-set x set)
  (cons x set))



(union-set '(1 2 3 4 5 6) '(1 2 3 4 5 6))
(adjoin-set 1 '(1 2 3))
(adjoin-set 1 '(4 5 6))
(intersection-set '(1 2 3 4 5 6) '(1 2 3 4 5 6))