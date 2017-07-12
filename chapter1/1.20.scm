#lang planet neil/sicp

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (my-remainder a b c)
  (if (or (= a 0) (= b 0))
      a
      (remainder a b)))

(define (gcd a b count)
  (if (= b 0)
      count
      (gcd b (my-remainder a b 0) (+ count 1))))

(define (gcd-apply a b count)
  (new-if (= b 0)
          count
          (gcd-apply b (my-remainder a b 0) (+ count 1))))

(gcd 206 40 0)
(gcd-apply 4 2 0)