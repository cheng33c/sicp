#lang planet neil/sicp

(define (cube x) (* x x x))
(define (round-to-next-even x) 
  (+ x (remainder x 2)))

(define (next-even n)
    (if (even? n)
        n
        (+ n 1)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define fixed-n (next-even n)) 
  (define h (/ (- b a) (next-even n)))
  (define (simpson-term k) 
    (define y (f (+ a (* k h)))) 
    (if (or (= k 0) (= k fixed-n)) 
        y
        (if (even? k) 
            (* 2 y) 
            (* 4 y)))) 
  (* (/ h 3) (sum simpson-term 0 inc fixed-n)))


(simpson cube 0 1 100)
(simpson cube 0 1 1000)