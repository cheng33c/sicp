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

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(sub-interval '(1 2 3) '(3 4 5))