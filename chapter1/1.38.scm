#lang planet neil/sicp

(define (cont-frac-iter n d k)
  (define (iter cur res)
    (cond ((= cur 0) res)
          ((= cur k)
           (iter (- cur 1) (/ (n cur) (d cur))))
          (else
           (iter (- cur 1) (/ (n cur)
                              (+ (d cur) res))))))
  (iter k 0))

(define (euler-continuis i)
  (if (= (remainder (+ i 1) 3) 0)
      (* 2 (/ 3 (+ i 1)))
      1))

(+ (cont-frac-iter (lambda (n) 1.0)
              euler-continuis
              500)
   2.0)