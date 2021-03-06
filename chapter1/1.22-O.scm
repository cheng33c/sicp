#lang planet neil/sicp

;;; 这个是标准答案的程序。 同样， 10000以上所需要的时间还是最少的
;;; 更诡异的是1000以上所需要的时间居然还是最多的。

(define (next-odd n)
    (if (odd? n)
        (+ 2 n)
        (+ 1 n)))

(define (square x) (* x x))
(define (prime? n)
    (= n (smallest-divisor n)))
(define (smallest-divisor n)
    (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
            n)
          ((divides? test-divisor n)
            test-divisor)
          (else
            (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
    (= (remainder b a) 0))

(define (continue-primes n count)
    (cond ((= count 0)
            (display "are primes."))
          ((prime? n)
            (display n)
            (newline)
            (continue-primes (next-odd n) (- count 1)))
          (else
            (continue-primes (next-odd n) count))))

(define (search-for-primes n)
    (let ((start-time (runtime)))
        (continue-primes n 3)
        (- (runtime) start-time)))

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)