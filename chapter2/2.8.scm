#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (define (iter y min)
    (cond ((null? y) min)
          ((number? y) y)
          ((< (car y) min)
           (iter (cdr y) (car y)))
          (else (iter (cdr y) min))))
  (iter (cdr x) (car x)))

(define (upper-bound x)
  (define (iter y max)
    (cond ((null? y) max)
          ((number? y) y)
          ((> (car y) max)
           (iter (cdr y) (car y)))
          (else (iter (cdr y) max))))
  (iter (cdr x) (car x)))

;;; 排除负数对结果的影响，用let取大小
(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(sub-interval '(1 2 3) '(3 4 5))
(sub-interval '(1 2 3) '(-3 -4 -5))