#lang planet neil/sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson term a b n)

  (define h (/ (- b a) n))
  (define (try-it k result)
    (cond ((> k n) result)
          ((or (= k n) (= k 0))
           (try-it (+ k 1) (+ result (term (+ a (* k h))))))
          (else (if (even? n)
                    (try-it (+ k 1) (+ result (* (term (+ a (* k h))) 4)))
                    (try-it (+ k 1) (+ result (* (term (+ a (* k h))) 2)))))))

  (if (even? n)
      (* (/ h 3) (try-it 0 0))
      #f))

(simpson cube 0 1 100)
(simpson cube 0 1 1000)