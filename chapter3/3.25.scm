(define (make-table)

  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((equal? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key-ls)
      (define (iter keys table)
	(cond ((null? keys) false)
	      ((null? (cdr keys))
	       (let ((record (assoc (car keys) (cdr table))))
		 (if record
		     (cdr record)
		     false)))
	      (else
	       (let ((subtable (assoc (car keys) (cdr table))))
		 (if subtable
		     (iter (cdr keys) subtable)
		     false)))))
      (iter key-ls local-table))
    (define (insert! key-ls value)
      (define (iter keys table)
	(cond ((null? table)
	       (if (null? (cdr keys))
		   (cons (car keys) value)
		   (list (car keys) (iter (cdr keys) '()))))
	      ((null? (cdr keys))
	       (let ((record (assoc (car keys) (cdr table))))
		 (if record
		     (set-cdr! record value)
		     (set-cdr! table (cons (cons (car keys) value)
				     (cdr table))))))
	      (else
	       (let ((subtable (assoc (car keys) (cdr table))))
		 (if subtable
		     (iter (cdr keys) subtable)
		     (set-cdr! table
			       (cons (list (car keys)
					   (iter (cdr keys)
						 '()))
				     (cdr table))))))))
      (iter key-ls local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else
	     (error "Unknow operation -- TABLE" m))))
    dispatch))

(define (lookup table . key-list) ((table 'lookup-proc) key-list))
(define (insert! table value . key-list) ((table 'insert-proc!) key-list value))
(define my (make-table))
