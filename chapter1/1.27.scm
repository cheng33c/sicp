#lang planet neil/sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (congruent? a n)
    (= (expmod a n n) a))

(define (prime? n) (fast-prime? n 100))

(define (carmichael n)
  (define (fermat-test a n)
    (cond ((= a n) #t)
          ((congruent? a n) (fermat-test (+ a 1) n))
          (else #f)))
  (fermat-test 2 n ))

(carmichael 561)
(carmichael 1105)
(carmichael 1729)
(carmichael 2465)
(carmichael 2821)
(carmichael 6601)