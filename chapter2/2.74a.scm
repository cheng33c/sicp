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
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

;; main
(define (apply-generic op name file)
  (let ((type-name (type-tag (car file)))) 
    (let ((proc (get op type-name)))
      (if proc
          (proc name file)
          (error "no result")))))

(define (install-employee-package)
  (define (get-record employee file)
    (if (null? file)
        (error "not found " employee)
        (if (eq? (get-name (car file)) employee)
            (car file)
            (get-record employee (cdr file)))))

  (define (get-name file)
    (cadr file))
  
  (put 'get-record 'employee get-record)
  (put 'get-name 'employee get-name)
  'done)


(install-employee-package)
(apply-generic 'get-record 'mark
               (list (list 'employee 'kevin 30)
                     (list 'employee 'mark 20)))