#lang planet neil/sicp

(define (accumulate combiner null-value term a next b)
  (define (iter a null-value)
    (if (> a b)
        null-value
        (iter (next a) (combiner (term a) null-value))))
  (iter a null-value))

(define (accumulate2 combiner null-value term a next b)
  (define (rec a null-value)
    (if (> a b)
        null-value
        (combiner a (rec (next a) null-value))))
  (rec a null-value))


;;; accumulate test
(define (next x) (+ x 1))
(define (identity x) x)
(accumulate * 1 identity 1 next 6)
(accumulate + 0 identity 1 next 6)

(accumulate2 * 1 identity 1 next 6)
(accumulate2 + 0 identity 1 next 6)

;;; define sum & product

(define (sum-iter term a next b)
  (accumulate + 0 term a next b))
(define (sum-rec term a next b)
  (accumulate2 + 0 term a next b))

(sum-iter identity 5 next 10)
(sum-rec identity 5 next 10)


(define (mul-iter term a next b)
  (accumulate * 1 term a next b))
(define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(* (product pi-term 1 next 1000) 4)