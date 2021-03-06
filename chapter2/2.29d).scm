#lang planet neil/sicp

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;; problem b)
(define (total-weight mobile)
  (define (loop item result)
    (cond ((number? item)
           (+ item result))
          ((null? item) 0)
          (else
           (+ result
              (loop (car item) result)
              (loop (cdr item) result)))))
  (loop mobile 0))

;; test problem b)
(total-weight
 (make-mobile (make-branch 10 25) (make-branch 5 20)))

;; problem c)
(define (branch-weight branch)
  (if (hangs-another-mobile? branch)              ; 如果分支吊着另一个活动体
      (total-weight (branch-structure branch))    ; 那么这个活动体的总重量就是这个分支的重量
      (branch-structure branch)))                 ; 否则, 分支的 structure 部分就是分支的重量

(define (hangs-another-mobile? branch)              ; 检查分支是否吊着另一个活动体
  (pair? (branch-structure branch)))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balance? branch)
  (let ((left (left-branch branch))
        (right (right-branch branch)))
    (eq?
     (branch-torque left)
     (branch-torque right))))

(define (same-torque? left right)
  (= (branch-torque left)
     (branch-torque right)))

(define (branch-balance? branch)
  (if (hangs-another-mobile? branch)              ; 如果分支上有子活动体
      (mobile-balance? (branch-structure branch))  ; 那么(递归地)检查子活动体的平衡性
      #t))   

(define (mobile-balance? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and
     (same-torque? left right)
     (branch-balance? left)
     (branch-balance? right))))

;; test problem c)
(define balance-mobile (make-mobile (make-branch 10 10)
                                    (make-branch 10 10)))
(mobile-balance? balance-mobile)
(define mobile-with-sub-mobile (make-mobile (make-branch 10 balance-mobile)
                                            (make-branch 10 balance-mobile)))
(mobile-balance? mobile-with-sub-mobile)
