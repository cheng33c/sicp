#lang planet neil/sicp

;; require
(define (square x) (* x x))
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
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2) ;; changed part
                    (error "can't coverse same type" type1) ;; changed part
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))
(define (apply-generic2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((higher (higher-type? type1 type2)))
                  (if (eq? type1 higher)
                      (raise (cadr args))
                      (raise (car args))))))))))
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (make-real x)
  ((get 'make 'real) x))
(define (make-integer x)
  ((get 'make 'integer) x))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (raise x)
  (apply-generic2 'raise x))
(define (raise2 x y)
  (apply-generic2 'raise x y))

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'tag 'integer tag)
  (put 'raise '(integer)
                (lambda (x) ((get 'make 'rational) x 1)))
  (put 'make 'integer
       (lambda (x) (tag x))))

(define (install-rational-package)
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
                (lambda (x) (make-real x))))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'tag 'real tag)
  (put 'make 'real (lambda (x) (tag x))))

;; main
(define (get-level type)
  (cond ((eq? type 'integer) 0)
        ((eq? type 'rational) 1)
        ((eq? type 'real) 2)
        ((eq? type 'complex) 3)))

(define (higher-type? type1 type2)
  (let ((level1 (get-level type1))
        (level2 (get-level type2)))
    (cond ((> level1 level2) type1)
          ((> level2 level1) type2)
          ((= level1 level2)
           (error "ERROR --- higher-type?")))))


(install-real-package)
(install-rational-package)
(install-integer-package)

(raise2 (make-integer 5) (make-rational 1 0))