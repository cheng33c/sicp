#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (define (iter y min)
    (cond ((null? y) min)
          ((< (car y) min)
           (iter (cdr y) (car y)))
          (else (iter (cdr y) min))))
  (iter x (car x)))

(define (upper-bound x)
  (define (iter y man)
    (cond ((null? y) man)
          ((> (car y) man)
           (iter (cdr y) (car y)))
          (else (iter (cdr y) man))))
  (iter x (car x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(add-interval '(1 2 3) '(3 4 5))