#lang sicp

;basic operations 
(define (square x) 
  (* x x)) 
(define (divides? a b) 
  (= (remainder b a) 0)) 
(define (even? n) 
  (= (remainder n 2) 0)) 
;smallest divisor computation 
(define (smallest-divisor n) 
  (define (find-divisor n test) 
    (cond ((> (square test) n) n) 
          ((divides? test n) test) 
          (else (find-divisor n (+ test 1))))) 
  (find-divisor n 2)) 
;primality check 
(define (prime? n) 
  (= n (smallest-divisor n)))

(define (timed-prime-test n) 
  (start-prime-test n (runtime))) 
(define (start-prime-test n start-time) 
  (if (prime? n) 
      (report-prime n (- (runtime) start-time)) 
      #f)) 
(define (report-prime n elapsed-time) 
  (display n) 
  (display "***") 
  (display elapsed-time) 
  (newline)) 
;search counter 
(define (search-for-primes n counter) 
  (if (even? n) 
      (s-f-p (+ n 1) counter) 
      (s-f-p n counter))) 
;;it's important to pay attention to the fact that predicate of the first
;;'if' here calls (timed-prime-test n) which in case of #t
;;computes into two procedures - (report-prime n (elapsed-time)) and 'then' case of the first 'if'.  
(define (s-f-p n counter) 
  (if (> counter 0) 
      (if (timed-prime-test n) 
          (s-f-p (+ n 2) (- counter 1)) 
          (s-f-p (+ n 2) counter)) 
      "COMPUTATION COMPLETE"))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
(search-for-primes 1000000000 3)
(search-for-primes 10000000000 3)
(search-for-primes 100000000000 3)
(search-for-primes 1000000000000 3)