#lang planet neil/sicp

;; require
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
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

;; e.g. (list 'Mark (list 'branch 'info) (list 'salary '$200))

(define (apply-generic op name file)
  (let ((type-name (type-tag (car file)))) 
    (let ((proc (get op type-name)))
      (if proc
          (proc name file)
          (error "no result")))))

(define (install-salary-package)

  (define (salary? employee-info)
    (cond ((null? employee-info)
           (error "can't find salary"))
          ((and (pair? (car employee-info))
                (eq? (car (car employee-info)) 'salary))
           (car employee-info))
          (else (salary? (cdr employee-info)))))
  (define (get-name pair) (cadr pair))

  (define (get-salary name file)
    (let ((employee (car file)))
      (if (eq? name (get-name employee))
          (salary? employee)
          (get-salary name (cdr file)))))

  (put 'salary? 'employee salary?)
  (put 'get-name 'employee get-name)
  (put 'get-salary 'employee get-salary)

  'done)

(install-salary-package)
(apply-generic 'get-salary 'Kevin
               (list
                (list 'employee 'Mark (list 'branch 'info) (list 'salary '$200))
                (list 'employee 'Kevin (list 'branch 'sale) (list 'salary '$500))))