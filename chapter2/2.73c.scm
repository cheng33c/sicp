#lang planet neil/sicp

;; get put methods
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
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; main
(define (install-pow-package)

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (base m) (car m))
  
  (define (exponent m) (cadr m))
  
  (define (make-exponentiation base exp)
    (cond ((= exp 0) 1)
          ((= exp 1) base)
          (else (list '** base exp))))
  
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

  (put 'make-product '** 'make-product)
  (put 'base '** 'base)
  (put 'exponent '** 'exponent)
  (put 'make-exponentiation '** 'make-exponentiation)
  
  (put 'deriv '**
       (lambda (exp var)
         (let ((u (base exp))
               (n (exponent exp)))
           (make-product n
                         (make-product
                          (make-exponentiation u (- n 1))
                          (deriv u var))))))
  'install-pow-package-done)

;; test
(install-pow-package)
(deriv '(** x 3) 'x)