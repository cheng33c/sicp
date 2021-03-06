#lang planet neil/sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (next-even n)
    (if (even? n)
        n
        (+ n 1)))

(define (simpson f a b n)
  
  (define h (/ (- b a) (next-even n)))

  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (if (or (= k 0) (= k (next-even n)))
        y
        (if (even? k)
            (* 2 y)
            (* 4 y))))

  (* (/ h 3) (sum simpson-term 0 inc (next-even n))))

(simpson cube 0 1 100)
(simpson cube 0 1 10000000)