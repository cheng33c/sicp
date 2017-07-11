#lang planet neil/sicp

(define (fast-except-iter b n)
  (define (even? n) (= (remainder n 2) 0))
  (define (fast-except-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-except-iter (* a a) b (/ n 2)))
          (else (fast-except-iter (* a b) b (- n 1)))))
  (fast-except-iter 1 b n))

(fast-except-iter 5 1000)