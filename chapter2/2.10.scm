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

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((upper (upper-bound y))
        (lower (lower-bound y)))
    (if (or (= upper 0) (= lower 0))
        (error "can't divide by 0")
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))


(add-interval '(1 2 3) '(-3 -4 -5))
(sub-interval '(1 2 3) '(-3 -4 -5))
(mul-interval '(1 2 3) '(-3 -4 -5))
(div-interval '(-1 2 3) '(0 4 5))