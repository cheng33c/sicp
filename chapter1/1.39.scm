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


(define (tan-cf x k)
    
    (define (N i)
        (if (= i 1)
            x
            (- (* x x))))

    (define (D i)
        (- (* i 2) 1))

    (exact->inexact (cont-frac-iter N D k)))


(tan-cf 1 100)