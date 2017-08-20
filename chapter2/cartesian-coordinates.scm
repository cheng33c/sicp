#lang planet neil/sicp

;; require
(define (square x) (* x x))

;; structure & select function
;; Cartesian coordinates
(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-real-imag (* (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (div-complex z1 z2)
  (make-from-real-imag (/ (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))