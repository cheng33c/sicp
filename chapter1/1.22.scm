#lang planet neil/sicp

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes begin)
  (define (search num)
    (if (prime? num)
        (timed-prime-test num)
        (search (+ num 2))))
  (if (= (remainder begin 2) 0)
      (search (+ begin 1))
      (search begin)))


(search-for-primes 1000)
(search-for-primes 10010)
(search-for-primes 100000)
(search-for-primes 1000000)

;;; 1009 *** 87 10007 *** 12 100003 *** 36 1000003 *** 110
;;; 1009 *** 40 10007 *** 11 100003 *** 33 1000003 *** 90
;;; 1009 *** 52 10007 *** 19 100003 *** 54 1000003 *** 165
;;; 1009 *** 47 10037 *** 14 100003 *** 56 1000003 *** 90


;;; 观察出计算时间和数的大小并没有关系
;;; 奇怪的是我计算出结果中 大于1000 的最小素数计算时间比 大于10000和大于100000 的大
;;; 即使我将 10000 改为 10010（注： 因为10037是素数，从10010计算要10037的步数花费较多）
;;; 仍是 大于1000 的答案计算的时间最久


(define (count-step begin)
  (define (search num)
    (if (prime? num)
        (smallest-divisor-step num)
        (search (+ num 2))))
  (if (= (remainder begin 2) 0)
      (search (+ begin 1))
      (search begin)))

(define (prime-step? n)
  (= n (smallest-divisor-step n)))

(define (smallest-divisor-step n)
  (define (find-divisor n test-divisor step)
    (cond ((> (square test-divisor) n) step)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1) (+ step 1)))))
  (find-divisor n 2 0))


;(count-step 1000)
;(count-step 10010)
;(count-step 100000)
;(count-step 1000000)

;;; 这个过程能计算某个数的步数，但不能计算某个阶段的步数
;;; 通过上面计算步数的过程看到， 1000以上的步数是最少的， 只有30步