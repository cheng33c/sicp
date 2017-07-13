#lang planet neil/sicp

;;; 这个程序非常慢。
;;; 费马检查在对一个非常大的数进行素数检测的时候,需要计算一个很大的乘幂
;;; 比如说,求十亿的一亿次方,这种非常大的数值计算的速度非常慢,而且很容易因为超出实现的限制而造成溢出。
;;; expmod 函数,通过每次对乘幂进行 remainder 操作,从而将乘幂限制在一个很小的范围内(不超过参数 m
;;; 这样可以最大限度地避免溢出,而且计算速度快得多


(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n) (fast-prime? n 100)) 

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))



(timed-prime-test 1009) 
(timed-prime-test 1013) 
(timed-prime-test 1019) 
(timed-prime-test 10007) 
(timed-prime-test 10009) 
(timed-prime-test 10037) 
(timed-prime-test 100003) 
(timed-prime-test 100019) 
(timed-prime-test 100043) 
(timed-prime-test 1000003) 
(timed-prime-test 1000033) 
(timed-prime-test 1000037)

(newline) 
(timed-prime-test 1000000007) 
(timed-prime-test 1000000009) 
(timed-prime-test 1000000021)