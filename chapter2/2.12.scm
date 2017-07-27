#lang planet neil/sicp

(define (make-interval lower-bound upper-bound)
  (cons lower-bound upper-bound))

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

(define (make-center-percent center percent)
  (let ((percent-value (* center percent)))
    (cons (- center percent-value)
          (+ center percent-value))))

(define (percent interval)
  (let ((lower (car interval))
        (upper (car (cdr interval))))
    (let ((center (/ (+ lower upper) 2)))
      (/ (- upper center) center))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(percent '(1 3))
(make-center-percent 2 (percent '(2 3)))
(make-center-percent 10 0.5)