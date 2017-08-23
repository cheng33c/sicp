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
(define (apply-generic op name file)
  (let ((type-name (type-tag (car file)))) 
    (let ((proc (get op type-name)))
      (if proc
          (proc name file)
          (error "no result")))))

;; main
(define (install-find-package)

  (define (get-name file)
    (cadr file))

  (define (get-record employee file)
    (if (null? file)
        (error "not found " employee)
        (if (eq? (get-name (car file)) employee)
            (car file)
            (get-record employee (cdr file)))))
  
  (define (find-employee-record name list)
    (define (iter result current-list)
      (cond ((null? current-list) result)
            ((and (pair? current-list)
                  (eq? (get-name (car current-list)) name))
             (iter
              (append result (get-record name current-list))
              (cdr current-list)))
            (else (iter result (cdr current-list)))))
    (iter '() list))

  (put 'find-employee-record 'employee find-employee-record)
  (put 'get-name 'employee get-name)
  (put 'get-record 'employee get-record)
  
  'done)


;; test
(install-find-package)
(display
 (apply-generic 'find-employee-record 'Kevin
               (list
                (list 'employee 'Mark (list 'branch 'info) (list 'salary '$200))
                (list 'employee 'Kevin (list 'branch 'sale) (list 'salary '$500))
                (list 'employee 'Bob (list 'branch 'sport) (list 'salary '$900))
                (list 'employee 'Evan (list 'branch 'info) (list 'salary '$600))
                (list 'employee 'Kevin (list 'branch 'clean) (list 'salary '$600)))))

;; d) 安装新的package