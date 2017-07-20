#lang planet neil/sicp

(define (con-frac n d k)
  (define (rec cur)
    (if (= cur k)
        (/ (n k) (d k))
        (/ (n cur) (+ (d cur) (rec (+ cur 1))))))
  (rec 1))

(con-frac (lambda (i) 1.0)
          (lambda (i) 1.0)
          100)


(define (con-frac-iter n d k)
  (define (iter cur res)
    (cond ((= cur 1) res)
          ((= cur k)
           (con-frac-iter (- cur 1) (/ (n cur) (/ d cur))))
          (else
           (con-frac-iter (- cur 1) (/ (n cur) (+ (d cur) res))))))
  (iter k 0))

(con-frac (lambda (i) 1.0)
          (lambda (i) 1.0)
          100)