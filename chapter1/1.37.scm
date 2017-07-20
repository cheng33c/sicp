#lang planet neil/sicp

(define (cont-frac n d k)
  (define (rec cur)
    (if (= cur k)
        (/ (n k) (d k))
        (/ (n cur) (+ (d cur) (rec (+ cur 1))))))
  (rec 1))

(cont-frac (lambda (i) 1.0)
          (lambda (i) 1.0)
          100)


(define (cont-frac-iter n d k)
  (define (iter cur res)
    (cond ((= cur 0) res)
          ((= cur k)
           (iter (- cur 1) (/ (n cur) (d cur))))
          (else
           (iter (- cur 1) (/ (n cur) (+ (d cur) res))))))
  (iter k 0))

(cont-frac-iter (lambda (i) 1.0)
               (lambda (i) 1.0)
               1000)