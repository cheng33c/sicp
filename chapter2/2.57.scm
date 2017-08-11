#lang planet neil/sicp

;; require
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (addend s) (cadr s))
(define (augend s) (cddr s))
(define (make-sum a1 a2)
  (list '+
        a1
        (if (> (length a2) 2)
            (make-sum (car a2) (cdr a2))
            (car a2))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))


(define (make-product m1 m2) (list '* m1 m2))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


(define (make-exponentiation base exp)
  (cond ((= exp 0) 1)
        ((= exp 1) base)
        (else (list '** base exp))))
(define (base m) (cadr m))
(define (exponent m) (caddr m))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))



;; main part 



(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ((u (base exp))
               (n (exponent exp)))
           (make-product n
                         (make-product
                          (make-exponentiation u (- n 1))
                          (deriv u var)))))
        (else
         (error "unknow expression type -- DERIV" exp))))


(deriv '(+ x 3 6) 'x)