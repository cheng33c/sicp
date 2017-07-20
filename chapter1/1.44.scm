#lang planet neil/sicp

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f times)
  (define (iter repeated-f time)
    (if (= time 1)
        repeated-f
        (iter (compose f repeated-f) (- time 1))))
  (iter f times))

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (- x dx) x (+ x dx)) 3)))

(define (smooth-n-times f times)
  (if (= times 0)
      f
      (smooth (smooth-n-times f (- times 1)))))

(define (smooth-n-times-iter f times)
  (define (iter smoothed-f time)
    (if (= time 0)
        smoothed-f
        (iter (smooth smoothed-f) (- time 1))))
  (iter f times))

(define (smooth-n-times3 f n)
  (define (iter i smoothed-f)
    (if (= i 0)
        smoothed-f
        (iter (- i 1)
              (smooth smoothed-f))))
  (iter n f))

((smooth-n-times3 square 10) 5)
((smooth-n-times-iter square 10) 5)
((smooth-n-times square 10) 5)


(define (n-fold-smooth f n) 
  ((repeated smooth n) f))

((n-fold-smooth square 10) 5)