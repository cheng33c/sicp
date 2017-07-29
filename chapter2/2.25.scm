#lang planet neil/sicp

(define x (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr x)))))

(define y (list (list 7)))
(car (car y))

;; 理解这个就有一点困难，可以用盒子指针结构画出z的图形
;; (mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons (mcons 4 (mcons (mcons 5 (mcons (mcons 6 (mcons 7 '())) '())) '())) '())) '())) '()))
;; 每次多了一个mcons是因为要多用一个盒子来指向其他盒子（即其他的列表）
;; 所以每次我们要脱一层的列表都要进行一次(car (cdr z))
(define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))))))

