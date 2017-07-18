#lang planet neil/sicp

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a null-value)
    (cond ((> a b) null-value)
          ((filter a) (iter (next a) (combiner (term a) null-value)))
          (else (iter (next a) null-value))))
  (iter a null-value))


;;; prime sum
(define (square x) (* x x))
(define (prime? n)
  (define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m)) m))
          (else
           (remainder (* base (expmod base (- exp 1) m)) m))))
  (fast-prime? n 100))

(define (sum-iter term a next b filter)
  (filtered-accumulate + 0 identity a next b filter))

(define (identity x) x)
(define (next x) (+ x 1))

(sum-iter identity 5 next 10 prime?)


;;; gcd
(define (gcd m n) 
  (cond ((< m n) (gcd n m))
        ((= n 0) m)
        (else (gcd n (remainder m n))))) 
  
(define (relative-prime? m n) 
  (= (gcd m n) 1)) 
  
(define (product-of-relative-primes n) 
  (define (filter x) 
    (relative-prime? x n)) 
  (filtered-accumulate * 1 identity 1 next n filter))

(product-of-relative-primes 5)