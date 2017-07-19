#lang planet neil/sicp

(define (fixed-point f first-guess)
  (define tolerance 0.0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;;; not use average
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)

(newline)
(newline)
;;; use average
(define (average x y) (/ (+ x y) 2))
(define (calog1000 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               2.0))
(calog1000 1000)

;;; 不使用平均阻尼大概需要30步
;;; 使用平均阻尼需要9步