#lang planet neil/sicp

(define (my-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (my-car z)
    (if (= 0 (remainder z 2))
        (+ 1 (my-car (/ z 2)))
        0))

(define (my-cdr z)
    (if (= 0 (remainder z 3))
        (+ 1 (my-cdr (/ z 3)))
        0))

(define (my-car-iter z)
  (define (iter val times)
    (if (= 0 (remainder val 2))
        (iter (/ val 2) (+ times 1))
        times))
  (iter z 0))

(define (my-cdr-iter z)
  (define (iter val times)
    (if (= 0 (remainder val 3))
        (iter (/ val 3) (+ times 1))
        times))
  (iter z 0))
        
(my-car-iter (my-cons 2 3))
(my-cdr-iter (my-cons 2 3))