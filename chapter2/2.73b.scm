#lang planet neil/sicp

;; required
;; get & put operations
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknow operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (attach-tag type-tag x y)
  (list type-tag x y))

(define (type-tag datumn)
  (car datumn))

(define (contents datumn)
  (cdr datumn))


;; problem provied

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; main

(define (install-sum-package)
  
  (define (addend s) (car s))

  (define (augend s) (cadr s))

  (define (make-sum x y)
    (cond ((=number? x 0)
           y)
          ((=number? y 0)
           x)
          ((and (number? x) (number? y))
           (+ x y))
          (else
           (attach-tag '+ x y))))

  (put 'addend '+ addend)
  (put 'augend '+ augend)
  (put 'make-sum '+ make-sum)

  (put 'deriv '+
       (lambda (exp var)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))))
  'done)

(define (make-sum x y)
  ((get 'make-sum '+) x y))

(define (addend sum)
  ((get 'addend '+) (contents sum)))

(define (augend sum)
  ((get 'augend '+) (contents sum)))

(define (install-product-package)
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  
  (put 'multiplier '* multiplier)
  (put 'multiplicand '* multiplicand)
  (put 'make-product '* make-product)
  (put 'deriv '* (lambda (exp var)
                   (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp)))))

;; test
(install-product-package)
(install-sum-package)

(deriv 0 'x)
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)