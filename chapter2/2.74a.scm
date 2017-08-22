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

;; main
(define (install-employee-package)
  (define (get-record file employee)
    (if (null? file)
        (error "not found " employee)
        (if (eq? (get-name (car file)) employee)
            (car file)
            (get-record (cdr file) employee))))

  (define (get-name file)
    (car file))
  
  (put 'get-record 'employee get-record)
  (put 'get-name 'employee get-name)
  'done)


(install-employee-package)
(get-record (list (list 'kevin 30) (list 'mark 20))
            'mark)