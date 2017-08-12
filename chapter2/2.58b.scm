#lang planet neil/sicp

;; bO的解法

;; require

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (make-exponentiation base exp)
  (cond ((= exp 0) 1)
        ((= exp 1) base)
        (else (list base '** exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponent m) (caddr m))

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



;; main part

(define (base m) (car m))
(define (sum? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


(define (addend s) (test car s))
(define (augend s) (test cddr s))
(define (product? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))
(define (multiplier p) (test car p))
(define (multiplicand p) (test cddr p))


(define (test location var)
  (define (simplify x)
    (cond
      ((sum? x) (make-sum (addend x) (augend x)))
      ((product? x) (make-product (multiplier x) (multiplicand x)))
      (else 
       (if (pair? x)
           (if (pair? (car x))
               (simplify (car x))
               (car x))
           x))))
  (let ((item (location var))) (simplify item)))


;; test part
(display (deriv '(x + 3 * (x + y + 2)) 'x))