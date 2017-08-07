#lang planet neil/sicp

(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (miller-rabin n)
  
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
           (remainder (* base (expmod base (- exp 1) m))
                      m))))

  (define (prime? n) 
    (define (fast-prime? n times)
      (cond ((= times 0) true)
            ((try-it n) (fast-prime? n (- times 1)))
            (else false)))
    (fast-prime? n 100))
  
  (define (try-it a)
    (if (= (expmod (+ 1 (random (- n 1))) (- n 1) n) 1)
        #t
        #f))

  (prime? n))


(miller-rabin 561)
(miller-rabin 561)
(miller-rabin 1105)
(miller-rabin 1729)
(miller-rabin 2465)
(miller-rabin 2821)
(miller-rabin 6601)
(newline)
(miller-rabin 1009)
(miller-rabin 1009) 
(miller-rabin 1013) 
(miller-rabin 1019) 
(miller-rabin 10007) 
(miller-rabin 10009) 
(miller-rabin 10037) 
(miller-rabin 100003) 
(miller-rabin 100019) 
(miller-rabin 100043) 
(miller-rabin 1000003) 
(miller-rabin 1000033) 
(miller-rabin 1000037)